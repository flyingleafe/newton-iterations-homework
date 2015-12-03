{-# LANGUAGE UnicodeSyntax, MultiParamTypeClasses #-}
module PointFree where

import Data.Maybe (mapMaybe)

-- to make terms shorter
𝕗 = flip

-- Some terms here have non-linear types (for example, φr uses argument x twice),
-- and I guess that means that we can't implement them using only (.) and flip,
-- so we will use this dup.
dup :: (a → a → b) → a → b
-- dup f x = f x x
-- dup = \f x → (f x) x
-- dup = \f x → ($x) (f x)
-- dup = \f x → ($x) (($x) f)
-- dup = \f x → (($x) . ($x)) f
-- dup = flip $ \x f → (($x) . ($x)) f
dup = flip $ \x → ($x).($x)
-- dup f x = f x x

φr :: Double → Double → Double
-- φr r x = r * x * (1 - x)
-- φr r = \x → r * x * (1 - x)
-- φr r = dup $ \x y → r * x * (1 - y)
-- φr r = dup $ \x y → (*) ((*) r x) (1 - y)
-- φr r = dup $ \x y → ((*) ((*) r x)) (1 - y)
-- φr r = dup $ \x y → ((*) ((*) r x)) ((1-) y)
-- φr r = dup $ \x y → (((*) ((*) r x)) . (1-)) y
-- φr r = dup $ \x → ((*) ((*) r x)) . (1-)
-- φr r = dup $ \x → (. (1-)) ((*) ((*) r x))
-- φr r = dup $ \x → (. (1-)) ((*) (((*) r) x))
-- φr r = dup $ \x → (. (1-)) (((*) . ((*) r)) x)
-- φr r = dup $ \x → ((. (1-)) . ((*) . ((*) r))) x
-- φr r = dup $ (. (1-)) . ((*) . ((*) r))
-- φr = \r → dup $ (. (1-)) . ((*) . ((*) r))
-- φr = \r → dup $ (.) (. (1-)) ((*) . ((*) r))
-- φr = \r → dup $ (.) (. (1-)) ((.) (*) ((*) r))
-- φr = \r → dup $ (.) (. (1-)) (((.) (*)) ((*) r))
-- φr = \r → dup $ (.) (. (1-)) ((((.) (*)) . (*)) r)
-- φr = \r → dup ((.) (. (1-)) ((((.) (*)) . (*)) r))
-- φr = \r → dup (((.) (. (1-))) ((((.) (*)) . (*)) r))
-- φr = \r → dup ((((.) (. (1-))) . (((.) (*)) . (*))) r)
-- φr = \r → (dup . (((.) (. (1-))) . (((.) (*)) . (*)))) r
-- φr = dup . (((.) (. (1-))) . (((.) (*)) . (*)))
-- φr = dup . (((. (1-)).) . (((*).) . (*)))
φr = dup . ((.(1-)).) . ((*).) . (*)
-- φr r x = r * x * (1 - x)

atrs :: Int → Int → Double → Double → [Double]
-- atrs post pres r start = take post $ drop pres $ iterate (φr r) start
-- atrs post pres r start = take post (drop pres (iterate (φr r) start))
-- atrs post pres r = take post . drop pres . iterate (φr r)
-- atrs post pres r = ((.) ((.) (take post) (drop pres))) (iterate (φr r))
-- atrs post pres r = ((.) ((.) (take post) (drop pres))) ((iterate . φr) r)
-- atrs post pres r = (((.) ((.) (take post) (drop pres))) . (iterate . φr)) r
-- atrs post pres = (. (iterate . φr)) ((.) ((.) (take post) (drop pres)))
-- atrs post pres = (. (iterate . φr)) ((.) (((.) (take post) . drop) pres))
-- atrs post pres = (. (iterate . φr)) (((.) . ((.) (take post) . drop)) pres)
-- atrs post pres = ((. (iterate . φr)) . ((.) . ((.) (take post) . drop))) pres
-- atrs post = (. (iterate . φr)) . ((.) . ((.) (take post) . drop))
-- atrs post = (. (iterate . φr)) . ((.) . (((.) (take post)) . drop))
-- atrs post = (. (iterate . φr)) . ((.) . ((.drop) ((.) (take post))))
-- atrs post = (. (iterate . φr)) . ((.) . (((.drop) . (.) . take) post))
-- atrs post = (. (iterate . φr)) . ((((.) .) . ((.drop) . (.) . take)) post)
-- atrs post = (((. (iterate . φr)) .) . (((.) .) . ((.drop) . (.) . take))) post
-- atrs = ((. (iterate . φr)) .) . ((.) .) . (.drop) . (.) . take
-- atrs = (.) ((.) ((.) ((.) ((. (iterate . φr)) .) ((.) .)) (.drop)) (.)) take
-- atrs = ((.) ((.) ((.) ((.) ((. (iterate . φr)) .) ((.) .)) (.drop)) (.))) take
-- atrs = ((.) ((.) ((.) ((.) ((. (iterate . φr)) .) ((.) .)) (𝕗 (.) drop)) (.))) take
-- atrs = ((.) ((.) (((.) ((.) ((. (iterate . φr)) .) ((.) .))) (𝕗 (.) drop)) (.))) take
-- atrs = ((.) ((.) ((((.) ((.) ((. (iterate . φr)) .) ((.) .))) . 𝕗 (.)) drop) (.))) take
-- atrs = ((.) (((.) ((((.) ((.) ((. (iterate . φr)) .) ((.) .))) . 𝕗 (.)) drop)) (.))) take
-- atrs = ((.) ((((.) . (((.) ((.) ((. (iterate . φr)) .) ((.) .))) . 𝕗 (.))) drop) (.))) take
-- atrs = ((.) (((.) . (((.) ((.) ((. (iterate . φr)) .) ((.) .))) . 𝕗 (.))) drop (.))) take
-- atrs = ((.) (𝕗 ((.) . (((.) ((.) ((. (iterate . φr)) .) ((.) .))) . 𝕗 (.))) (.) drop)) take
-- atrs = ((.) ((𝕗 ((.) . (((.) ((.) ((. (iterate . φr)) .) ((.) .))) . 𝕗 (.))) (.)) drop)) take
-- atrs = (((.) . (𝕗 ((.) . (((.) ((.) ((. (iterate . φr)) .) ((.) .))) . 𝕗 (.))) (.))) drop) take
-- atrs = ((.) . (𝕗 ((.) . (((.) ((.) ((. (iterate . φr)) .) ((.) .))) . 𝕗 (.))) (.))) drop take
-- atrs = ((.) . (𝕗 ((.) . (((.) ((.) ((.) (𝕗 (.) (iterate . φr))) ((.) .))) . 𝕗 (.))) (.))) drop take
-- atrs = ((.) . (𝕗 ((.) . (((.) ((.) (((.) . 𝕗 (.)) (iterate . φr)) ((.) .))) . 𝕗 (.))) (.))) drop take
-- atrs = ((.) . (𝕗 ((.) . (((.) (((.) (((.) . 𝕗 (.)) (iterate . φr))) ((.) .))) . 𝕗 (.))) (.))) drop take
-- atrs = ((.) . (𝕗 ((.) . (((.) ((((.) . ((.) . 𝕗 (.))) (iterate . φr)) ((.) .))) . 𝕗 (.))) (.))) drop take
-- atrs = ((.) . (𝕗 ((.) . (((.) (((.) . ((.) . 𝕗 (.))) (iterate . φr) ((.) .))) . 𝕗 (.))) (.))) drop take
-- atrs = ((.) . (𝕗 ((.) . (((.) (𝕗 ((.) . ((.) . 𝕗 (.))) ((.) .) (iterate . φr))) . 𝕗 (.))) (.))) drop take
-- atrs = ((.) . (𝕗 ((.) . (((.) ((𝕗 ((.) . ((.) . 𝕗 (.))) ((.) .)) (iterate . φr))) . 𝕗 (.))) (.))) drop take
-- atrs = ((.) . (𝕗 ((.) . ((((.) . (𝕗 ((.) . ((.) . 𝕗 (.))) ((.) .))) (iterate . φr)) . 𝕗 (.))) (.))) drop take
-- atrs = ((.) . (𝕗 ((.) . ((.𝕗 (.)) (((.) . (𝕗 ((.) . ((.) . 𝕗 (.))) ((.) .))) (iterate . φr)))) (.))) drop take
-- atrs = ((.) . (𝕗 ((.) . (((.𝕗 (.)) . ((.) . (𝕗 ((.) . ((.) . 𝕗 (.))) ((.) .)))) (iterate . φr))) (.))) drop take
-- atrs = ((.) . (𝕗 (((.) .) (((.𝕗 (.)) . ((.) . (𝕗 ((.) . ((.) . 𝕗 (.))) ((.) .)))) (iterate . φr))) (.))) drop take
-- atrs = ((.) . (𝕗 ((((.) .) . ((.𝕗 (.)) . ((.) . (𝕗 ((.) . ((.) . 𝕗 (.))) ((.) .))))) (iterate . φr)) (.))) drop take
-- atrs = ((.) . ((𝕗 ((((.) .) . ((.𝕗 (.)) . ((.) . (𝕗 ((.) . ((.) . 𝕗 (.))) ((.) .))))) (iterate . φr))) (.))) drop take
-- atrs = ((.) . (((𝕗 . (((.) .) . ((.𝕗 (.)) . ((.) . (𝕗 ((.) . ((.) . 𝕗 (.))) ((.) .)))))) (iterate . φr)) (.))) drop take
-- atrs = ((.) . ((𝕗 (𝕗 . (((.) .) . ((.𝕗 (.)) . ((.) . (𝕗 ((.) . ((.) . 𝕗 (.))) ((.) .)))))) (.) (iterate . φr)))) drop take
-- atrs = ((.) . (((𝕗 (𝕗 . (((.) .) . ((.𝕗 (.)) . ((.) . (𝕗 ((.) . ((.) . 𝕗 (.))) ((.) .)))))) (.)) (iterate . φr)))) drop take
-- atrs = ((.) . ((𝕗 (𝕗 . (((.) .) . ((.𝕗 (.)) . ((.) . (𝕗 ((.) . ((.) . 𝕗 (.))) ((.) .)))))) (.)) (iterate . φr))) drop take
-- atrs = (((.) .) ((𝕗 (𝕗 . (((.) .) . ((.𝕗 (.)) . ((.) . (𝕗 ((.) . ((.) . 𝕗 (.))) ((.) .)))))) (.)) (iterate . φr))) drop take
-- atrs = ((((.) .) . (𝕗 (𝕗 . (((.) .) . ((.𝕗 (.)) . ((.) . (𝕗 ((.) . ((.) . 𝕗 (.))) ((.) .)))))) (.))) (iterate . φr)) drop take
-- atrs = ((((.) .) . (𝕗 (𝕗 . (((.) .) . ((.𝕗 (.)) . ((.) . (𝕗 ((.) . ((.) . 𝕗 (.))) ((.) .)))))) (.))) (((.) iterate) φr)) drop take
-- atrs = ((((.) .) . (𝕗 (𝕗 . (((.) .) . ((.𝕗 (.)) . ((.) . (𝕗 ((.) . ((.) . 𝕗 (.))) ((.) .)))))) (.))) . ((.) iterate)) φr drop take
-- atrs = (((((.) .) . (𝕗 (𝕗 . (((.) .) . ((.𝕗 (.)) . ((.) . (𝕗 ((.) . ((.) . 𝕗 (.))) ((.) .)))))) (.))) .) ((.) iterate)) φr drop take
-- atrs = ((((((.) .) . (𝕗 (𝕗 . (((.) .) . ((.𝕗 (.)) . ((.) . (𝕗 ((.) . ((.) . 𝕗 (.))) ((.) .)))))) (.))) .) . (.)) iterate) φr drop take
-- atrs = (((((.) .) . (𝕗 (𝕗 . (((.) .) . ((.𝕗 (.)) . ((.) . (𝕗 ((.) . ((.) . 𝕗 (.))) ((.) .)))))) (.))) .) . (.)) iterate φr drop take
-- atrs = (((((.) .) . (𝕗 (𝕗 . (((.) .) . ((.𝕗 (.)) . ((.) . (𝕗 ((.) . ((.) . 𝕗 (.))) ((.) .)))))) (.))) .) . (.)) iterate φr drop take
atrs = (((((.).).(𝕗(𝕗.((.).).(.𝕗(.)).(.).(𝕗((.).(.).𝕗(.))((.).)))(.))).).(.)) iterate φr drop take
-- atrs post pres r start = take post $ drop pres $ iterate (φr r) start

next :: (Double → Double) → (Double → Double) → Double → Double
-- next f f' z = z - f z / f' z
-- next f f' z = z - f z / f' z
-- next f f' z = ((-) z) (((/) (f z)) (f' z))
-- next f f' z = ((-) z) ((((/) (f z)) . f') z)
-- next f f' z = (((-) z) . (((/) (f z)) . f')) z
-- next f f' z = dup (\x y → (((-) x) . (((/) (f x)) . f')) y) z
-- next f f' = dup (\x y → (((-) x) . (((/) (f x)) . f')) y)
-- next f f' = dup (\x → ((-) x) . (((/) (f x)) . f'))
-- next f f' = dup (\x → (.) ((-) x) (((/) (f x)) . f'))
-- next f f' = dup (\x → (.) ((-) x) (flip (.) f' ((/) (f x))))
-- next f f' = dup (\x → (.) ((-) x) (flip (.) f' (((/) . f) x)))
-- next f f' = dup (\x → (.) ((-) x) (((flip (.) f') . ((/) . f)) x))
-- next f f' = dup (\x → (((.) ((-) x)) . ((flip (.) f') . ((/) . f))) x)
-- next f f' = dup (\x → dup (\y z → (((.) ((-) y)) . ((flip (.) f') . ((/) . f))) z) x)
-- next f f' = dup (dup (\y z → (((.) ((-) y)) . ((flip (.) f') . ((/) . f))) z))
-- next f f' = dup (dup (\y → (((.) ((-) y)) . ((flip (.) f') . ((/) . f)))))
-- next f f' = dup (dup (\y → ((((.) . (-)) y) . ((flip (.) f') . ((/) . f)))))
-- next f f' = dup (dup (\y → (flip (.) ((flip (.) f') . ((/) . f)) (((.) . (-)) y))))
-- next f f' = dup (dup (\y → ((flip (.) ((flip (.) f') . ((/) . f))) . ((.) . (-))) y))
-- next f f' = dup (dup ((flip (.) ((flip (.) f') . ((/) . f))) . ((.) . (-))))
-- next f f' = dup (dup (flip (.) ((.) . (-)) (flip (.) ((flip (.) f') . ((/) . f)))))
-- next f f' = dup (dup (flip (.) ((.) . (-)) (flip (.) (flip (.) ((/) . f) (flip (.) f')))))
-- next f f' = dup (dup (flip (.) ((.) . (-)) (flip (.) (((flip (.) ((/) . f)) . (flip (.))) f'))))
-- next f f' = dup (dup (flip (.) ((.) . (-)) (((flip (.)) . ((flip (.) ((/) . f)) . (flip (.)))) f')))
-- next f f' = dup (dup (((flip (.) ((.) . (-))) . ((flip (.)) . ((flip (.) ((/) . f)) . (flip (.))))) f'))
-- next f f' = dup ((dup . ((flip (.) ((.) . (-))) . ((flip (.)) . ((flip (.) ((/) . f)) . (flip (.)))))) f')
-- next f f' = (dup . (dup . ((flip (.) ((.) . (-))) . ((flip (.)) . ((flip (.) ((/) . f)) . (flip (.))))))) f'
-- next f = (dup . (dup . ((flip (.) ((.) . (-))) . ((flip (.)) . ((flip (.) ((/) . f)) . (flip (.)))))))
-- next f = (dup . (dup . ((flip (.) ((.) . (-))) . ((flip (.)) . ((((flip (.)) . ((.) (/))) f) . (flip (.)))))))
-- next f = (dup . (dup . ((flip (.) ((.) . (-))) . ((flip (.)) . (flip (.) (flip (.)) (((flip (.)) . ((.) (/))) f))))))
-- next f = (dup . (dup . ((flip (.) ((.) . (-))) . ((flip (.)) . ((flip (.) (flip (.)) . ((flip (.)) . ((.) (/)))) f)))))
-- next f = (dup . (dup . ((flip (.) ((.) . (-))) . ((.) (flip (.)) ((flip (.) (flip (.)) . ((flip (.)) . ((.) (/)))) f)))))
-- next f = (dup . (dup . ((flip (.) ((.) . (-))) . (((.) (flip (.)) . (flip (.) (flip (.)) . ((flip (.)) . ((.) (/))))) f))))
-- next f = (dup . (dup . ((.) (flip (.) ((.) . (-))) (((.) (flip (.)) . (flip (.) (flip (.)) . ((flip (.)) . ((.) (/))))) f))))
-- next f = (dup . (dup . ((((.) (flip (.) ((.) . (-)))) . ((.) (flip (.)) . (flip (.) (flip (.)) . ((flip (.)) . ((.) (/)))))) f)))
-- next f = (dup . ((((.) dup) . (((.) (flip (.) ((.) . (-)))) . ((.) (flip (.)) . (flip (.) (flip (.)) . ((flip (.)) . ((.) (/))))))) f))
-- next f = (((.) dup) . (((.) dup) . (((.) (flip (.) ((.) . (-)))) . ((.) (flip (.)) . (flip (.) (flip (.)) . ((flip (.)) . ((.) (/)))))))) f
-- next = (((.) dup) . (((.) dup) . (((.) (flip (.) ((.) . (-)))) . ((.) (flip (.)) . (flip (.) (flip (.)) . ((flip (.)) . ((.) (/))))))))
-- next = (((.) dup) . (((.) dup) . (((.) (𝕗 (.) ((.) . (-)))) . ((.) (𝕗 (.)) . (𝕗 (.) (𝕗 (.)) . ((𝕗 (.)) . ((.) (/))))))))
next = (dup.) . (dup.) . ((𝕗(.)((.).(-))).) . (𝕗(.).) . 𝕗(.)(𝕗(.)) . 𝕗(.) . ((/).)
-- next f f' z = z - f z / f' z

takeFirst :: (a → Maybe b) → [a] → (Int, b)
takeFirst f l = head $ mapMaybe (foo f) (zip [0..] l) where
  foo :: (a -> Maybe b) → (Int, a) → Maybe (Int, b)
  -- foo f (x, y) = case f y of { Nothing → Nothing ; Just a → Just (x, a) }
  -- -- sequenceA :: Applicative m ⇒ (e, m a) → m (e, a)
  -- foo f (x, y) = sequenceA $ case f y of { Nothing → (x, Nothing) ; Just a → (x, Just a) }
  -- foo f (x, y) = sequenceA $ (x, case f y of { Nothing → Nothing ; Just a → Just a })
  -- -- (<$>) :: (a → b) → (e, a) → (e, b)
  -- foo f (x, y) = sequenceA $ (f <$> (x, y))
  -- foo f (x, y) = sequenceA $ ((f <$>) (x, y))
  -- foo f (x, y) = (sequenceA . (f <$>)) (x, y)
  -- foo f = sequenceA . (f <$>)
  foo = (sequenceA .) . (<$>)
