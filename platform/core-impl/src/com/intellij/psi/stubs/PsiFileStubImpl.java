// Copyright 2000-2024 JetBrains s.r.o. and contributors. Use of this source code is governed by the Apache 2.0 license.

/*
 * @author max
 */
package com.intellij.psi.stubs;

import com.intellij.lang.Language;
import com.intellij.openapi.diagnostic.Logger;
import com.intellij.openapi.util.Pair;
import com.intellij.psi.FileViewProvider;
import com.intellij.psi.PsiFile;
import com.intellij.psi.impl.source.PsiFileImpl;
import com.intellij.psi.impl.source.PsiFileWithStubSupport;
import com.intellij.psi.tree.IElementType;
import com.intellij.psi.tree.IStubFileElementType;
import com.intellij.util.SmartList;
import org.jetbrains.annotations.ApiStatus;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.Arrays;
import java.util.List;

public class PsiFileStubImpl<T extends PsiFile> extends StubBase<T> implements PsiFileStub<T> {
  @ApiStatus.Internal
  public static final IStubFileElementType TYPE = new IStubFileElementType(Language.ANY);

  private volatile T myFile;
  private volatile String myInvalidationReason;
  private volatile PsiFileStub<?>[] myStubRoots;

  public PsiFileStubImpl(T file) {
    super(null, null);
    myFile = file;
  }

  @Override
  public T getPsi() {
    return myFile;
  }

  @Override
  public void setPsi(@NotNull T psi) {
    myFile = psi;
  }

  public void clearPsi(@NotNull String reason) {
    myInvalidationReason = reason;
    myFile = null;
  }

  @Override
  public @Nullable String getInvalidationReason() {
    return myInvalidationReason;
  }

  @Override
  public IStubElementType getStubType() {
    return null;
  }

  @ApiStatus.Experimental
  @Override
  public IElementType getElementType() {
    return null;
  }

  @ApiStatus.Experimental
  @Override
  public ObjectStubSerializer<?, ? extends Stub> getStubSerializer() {
    return null;
  }

  @Override
  public @NotNull IStubFileElementType<?> getType() {
    return TYPE;
  }

  /** Don't call this method, it's public for implementation reasons */
  @ApiStatus.Internal
  public PsiFileStub<?> @NotNull [] getStubRoots() {
    if (myStubRoots != null) return myStubRoots;

    T psi = getPsi();
    if (psi == null) {
      return new PsiFileStub[]{this};
    }

    FileViewProvider viewProvider = psi.getViewProvider();
    PsiFile stubBindingRoot = viewProvider.getStubBindingRoot();

    StubTree baseTree = getOrCalcStubTree(stubBindingRoot);
    if (baseTree != null) {
      List<PsiFileStub<?>> roots = new SmartList<>(baseTree.getRoot());
      List<Pair<LanguageStubDescriptor, PsiFile>> stubbedRoots = StubTreeBuilder.getStubbedRootDescriptors(viewProvider);
      for (Pair<LanguageStubDescriptor, PsiFile> stubbedRoot : stubbedRoots) {
        if (stubbedRoot.second == stubBindingRoot) continue;
        StubTree secondaryStubTree = getOrCalcStubTree(stubbedRoot.second);
        if (secondaryStubTree != null) {
          PsiFileStub<?> root = secondaryStubTree.getRoot();
          roots.add(root);
        }
      }
      PsiFileStub<?>[] rootsArray = roots.toArray(EMPTY_ARRAY);
      for (PsiFileStub<?> root : rootsArray) {
        if (root instanceof PsiFileStubImpl) {
          ((PsiFileStubImpl<?>)root).setStubRoots(rootsArray);
        }
      }

      myStubRoots = rootsArray;
      return rootsArray;
    }
    return EMPTY_ARRAY;
  }

  private static StubTree getOrCalcStubTree(PsiFile stubBindingRoot) {
    StubTree result = null;
    if (stubBindingRoot instanceof PsiFileWithStubSupport) {
      result = ((PsiFileWithStubSupport)stubBindingRoot).getStubTree();
      if (result == null && stubBindingRoot instanceof PsiFileImpl) {
        result = ((PsiFileImpl)stubBindingRoot).calcStubTree();
      }
    }
    return result;
  }

  public void setStubRoots(PsiFileStub<?> @NotNull [] roots) {
    if (roots.length == 0) {
      Logger.getInstance(getClass()).error("Incorrect psi file stub roots count" + this);
    }
    myStubRoots = roots;
  }

  public boolean rootsAreSet() {
    return myStubRoots != null;
  }

  public final String getDiagnostics() {
    ObjectStubTree<?> stubTree = ObjectStubTree.getStubTree(this);
    T file = myFile;
    return this + "(" +
           "file='" + file + '\'' +
           ", invalidationReason=" + myInvalidationReason +
           ", stubRoots='" + Arrays.toString(myStubRoots) + '\'' +
           ", stubTree='" + stubTree + '\'' +
           ')';
  }
}