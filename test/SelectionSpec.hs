module SelectionSpec (spec) where

import Test.Hspec
import Selection

doc :: [String]
doc = ["cube(1, 1, 1)", "", "sphere(2)"]

spec :: Spec
spec = do
    describe "selectedText" $ do
        it "extracts a range within one line" $
            selectedText doc (0, 0) (0, 4) `shouldBe` "cube"
        it "works with the anchor after the cursor" $
            selectedText doc (0, 4) (0, 0) `shouldBe` "cube"
        it "spans multiple lines, including empty ones" $
            selectedText doc (0, 5) (2, 6) `shouldBe` "1, 1, 1)\n\nsphere"
        it "clamps out-of-range positions" $
            selectedText doc (2, 0) (9, 99) `shouldBe` "sphere(2)"

    describe "deleteRange" $ do
        it "joins the remainder of the first and last lines" $
            deleteRange doc (0, 4) (2, 6) `shouldBe` (["cube(2)"], (0, 4))
        it "is a no-op for an empty range" $
            deleteRange doc (1, 0) (1, 0) `shouldBe` (doc, (1, 0))
        it "removes a whole line including its newline" $
            deleteRange doc (1, 0) (2, 0) `shouldBe` (["cube(1, 1, 1)", "sphere(2)"], (1, 0))

    describe "lineSelection" $ do
        let sel = Just ((0, 5), (2, 6))
        it "covers from the start column to end of line on the first line" $
            lineSelection sel 0 (head doc) `shouldBe` Just (5, 13, True)
        it "marks a fully selected empty line" $
            lineSelection sel 1 "" `shouldBe` Just (0, 0, True)
        it "stops at the cursor column on the last line" $
            lineSelection sel 2 "sphere(2)" `shouldBe` Just (0, 6, False)
        it "is Nothing outside the range or for an empty selection" $ do
            lineSelection sel 3 "x" `shouldBe` Nothing
            lineSelection (Just ((1, 1), (1, 1))) 1 "abc" `shouldBe` Nothing

    describe "applySelection" $ do
        it "splits spans at the selection boundaries" $
            applySelection "S" (Just (2, 6, False)) [("a", "cube"), ("b", "(1)")]
                `shouldBe` [("a", "cu"), ("S", "be"), ("S", "(1"), ("b", ")")]
        it "adds a trailing selected cell for a selected newline" $
            applySelection "S" (Just (0, 0, True)) [] `shouldBe` [("S", " ")]
        it "leaves spans untouched with no selection" $
            applySelection "S" Nothing [("a", "x")] `shouldBe` [("a", "x")]
