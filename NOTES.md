
Principles
----------

## Robustness

There are two ways of approaching the annotations. The one, as used
in HSE, is to define the positions so that the original source can be
reconstructed.

The more useful/difficult option is to defined the annotations so that
they can be used to reconstruct a properly formatted source file even
when some major manipulations have been done to the AST. The means
everything has to be relative, similar to the pretty-print library.


## Mechanics

Each AST element/annotation must be self-contained, in that it can be
rendered correctly relative to a (positive) starting (row,col)
position at the top left, regardless of what these are.

Positions are all stored as instances of DeltaPos, which gives the row
and column offset for the next element relative to the *current
position*.

The *current position* is stored in the rendering state, and may be
updated by either explicitly moving forward, or by printing at an
*absolute position*.

The existing HSE ExactPrint rendering engine is used, which makes use
of absolute positions.

Utility functions exist to print at a DeltaPos, which simply combine
the *current position* with the delta to provide an *absolute
position*.

This combining takes place in the exactP/exactPC instances, and it is
up to the annotation generator and using instance to define the
reference point.

Two options exist

a) Relative to the SrcSpan defining the Located AST element.
b) Relative to the last output position

Option (a) makes sense for specific layout items that need vertical
alignment, while option (b) makes sense for spacing withing say an
expression where the name of one of the variables may eventually be
changed.

Rule of thumb: use option (b) unless option (a) is required, it should
result in a more resilient annotation when tools need to change
things.

## Comments

Comments should be pushed as low as possible down the AST.

Each annotation maintains a list of DComments, which are comments
defined as being relative to the start of the containing AST element
SrcSpan.

The rendering engine has an ordered list of comments which it inserts
into the output stream as it advances.

Every time a new AST element/annotation is entered, the current
position is taken as the start of the SrcSpan, and so the absolute
positions of the comment can be determined. These are then merged into
the list of comments for rendering.

Question: what happens if a sub-element changes size from the
original? Later comments will then be in a different position.

Perhaps provide a list of lists, relative to the sub-items.


### Mechanics of comments

Option 1.

When generating the annotation, pass the remaining list of comments to
the sub elements. Each sub-element (recursively) grabs the comments it
takes responsibility for, and passes the balance back. Any comments
fitting into the span but not allocated to a sub-element become
allocated at this level, interspersed between the sub elements.

Option 2.

Do a hard separation of comments according to SrcSpan. So the parent
element takes all comments in the parent SrcSpan that do not fit into
any of the sub-element SrcSpans.

This has the disadvantage that a syntax element can become
disconnected from its comments, e.g. a pre or post comment for a
declaration.



Note: Because the position always moves forward when emitting source,
and syntax element start points are used by exactPC to advance, there
is a problem with preceding comments.



### RenamedSource vs ParsedSource

It is easier to transform the AST for `RenamedSource`, as the names are
fully disambiguated.  However, the Name value no longer has the detail
of the original RdrName in terms of round tripping the source.

So there are three options

1. Capture the `Located RdrName` values in a separate map when doing
   the initial `annotateAST` pass, so they can be used for generating
   output using the `RenamedSource`.

2. Perform a pass over the `RenamedSource` to capture all the `Located
   Name` values so that they are available in AST manipulation code,
   and generate output from the `ParsedSource`.

3. Capture the `Located RdrName` values as part of the `SrcSpan`
   annotation for the `RdrName` `AnnotateP` instance, and generate
   output from the `RenamedSource`.

Option 1 has two advantages, (1) we are already doing a full traversal
over the `ParsedSource`, so it is relatively cheap to pick up the
`Located RdrName`s, and (2) The AST manipulator should not have to
write AST update code that has to call out to a separate API to
identify names.

Option 2 is in some senses simpler, and can work even if the renamer
fails.

Option 3 seems most workable, given the existing infrastructure to
store an arbitrary `Value` in the `Annotation`.

### ConDecl / PrefixCon changes through renamer

It goes from


          (PrefixCon
           [])
          (ResTyGADT {tests/examples/DataDecl.hs:21:16-26}
           (L {tests/examples/DataDecl.hs:21:16-26}
           (L {tests/examples/DataDecl.hs:21:16-26}
            (HsFunTy
             (L {tests/examples/DataDecl.hs:21:16-18}
              (HsTyVar
               (Unqual {OccName: Int})))
             (L {tests/examples/DataDecl.hs:21:23-26}
              (HsAppTy
               (L {tests/examples/DataDecl.hs:21:23-24}
                (HsTyVar
                 (Unqual {OccName: G1})))
               (L {tests/examples/DataDecl.hs:21:26}
                (HsTyVar
                 (Unqual {OccName: a}))))))))

to


           (PrefixCon
            [
             (L {tests/examples/DataDecl.hs:21:16-18}
              (HsTyVar {Name: Int}))])
           (ResTyGADT {tests/examples/DataDecl.hs:21:16-26}
            (L {tests/examples/DataDecl.hs:21:23-26}
             (HsAppTy
              (L {tests/examples/DataDecl.hs:21:23-24}
               (HsTyVar {Name: G1}))
              (L {tests/examples/DataDecl.hs:21:26}
               (HsTyVar {Name: a})))))

So

    PrefixCon []
    ...
    HsFunTy t1 t2

becomes

    PrefixCon [t1]
    ..
    t2

For a ResTyGADT declaration the PrefixCon will always have [] when parsed,
so can be used to distinguish Parsed from Renamed.
