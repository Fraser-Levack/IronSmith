-- | Pure text-selection helpers for the code editor.
--
--   Positions are @(row, col)@, zero-based, matching Brick's
--   'Brick.Widgets.Edit.getCursorPosition'. A selection is an anchor plus
--   the cursor; either may come first, so everything goes through
--   'orderedRange'. The end position is exclusive.
--
--   Kept free of Brick/Vty so it can be unit tested directly.
module Selection
    ( Pos
    , orderedRange
    , selectedText
    , deleteRange
    , lineSelection
    , applySelection
    ) where

type Pos = (Int, Int)

-- | Order two positions so the first is the earlier one in the document.
orderedRange :: Pos -> Pos -> (Pos, Pos)
orderedRange a b = if a <= b then (a, b) else (b, a)

-- | Clamp a position to the document so stale anchors can't index out of
--   bounds (e.g. after the text was replaced underneath them).
clampPos :: [String] -> Pos -> Pos
clampPos [] _ = (0, 0)
clampPos ls (r, c) =
    let r' = max 0 (min (length ls - 1) r)
        c' = max 0 (min (length (ls !! r')) c)
    in (r', c')

-- | The text between two positions, with lines joined by @\\n@.
selectedText :: [String] -> Pos -> Pos -> String
selectedText ls a b =
    let ((r1, c1), (r2, c2)) = orderedRange (clampPos ls a) (clampPos ls b)
    in if r1 == r2
           then take (c2 - c1) (drop c1 (ls !! r1))
           else concat
               [ drop c1 (ls !! r1), "\n"
               , concatMap (++ "\n") (take (r2 - r1 - 1) (drop (r1 + 1) ls))
               , take c2 (ls !! r2)
               ]

-- | Remove the text between two positions. Returns the new lines and the
--   position the cursor should land on (the start of the removed range).
deleteRange :: [String] -> Pos -> Pos -> ([String], Pos)
deleteRange [] _ _ = ([""], (0, 0))
deleteRange ls a b =
    let (start@(r1, c1), (r2, c2)) = orderedRange (clampPos ls a) (clampPos ls b)
        joined = take c1 (ls !! r1) ++ drop c2 (ls !! r2)
    in (take r1 ls ++ [joined] ++ drop (r2 + 1) ls, start)

-- | Which columns of line @row@ (whose text is @s@) fall inside the
--   selection: @Just (fromCol, toCol, includesNewline)@. The newline flag
--   lets the renderer highlight a trailing cell so fully-selected and
--   empty lines are visibly part of a multi-line selection.
lineSelection :: Maybe (Pos, Pos) -> Int -> String -> Maybe (Int, Int, Bool)
lineSelection Nothing _ _ = Nothing
lineSelection (Just (a, b)) row s
    | a == b                 = Nothing
    | row < r1 || row > r2   = Nothing
    | otherwise              = Just (from, to, row < r2)
  where
    ((r1, c1), (r2, c2)) = orderedRange a b
    from = if row == r1 then c1 else 0
    to   = if row == r2 then min c2 (length s) else length s

-- | Overlay a selection onto a line's highlighted spans. Each span is
--   @(attr, text)@; selected characters get @selAttr@ instead of their own
--   attribute. A selected newline becomes a trailing selected space.
applySelection :: attr -> Maybe (Int, Int, Bool) -> [(attr, String)] -> [(attr, String)]
applySelection _ Nothing spans = spans
applySelection selAttr (Just (from, to, nl)) spans =
    go 0 spans ++ [ (selAttr, " ") | nl ]
  where
    go _ [] = []
    go col ((a, t) : rest) =
        let len   = length t
            end   = col + len
            lo    = max 0 (from - col)
            hi    = min len (to - col)
            parts
                | end <= from || col >= to = [(a, t)]
                | otherwise =
                    filter (not . null . snd)
                        [ (a, take lo t)
                        , (selAttr, take (hi - lo) (drop lo t))
                        , (a, drop hi t)
                        ]
        in parts ++ go end rest
