||| This is the untyped lambda calculus with De Bruijn indices for variables.
module Main

data Expr : Type where
    EVar : Nat -> Expr
    EAbs : Expr -> Expr
    EApp : Expr -> Expr -> Expr

Show Expr where
    show (EVar x) = show x
    show (EApp e1 e2) = show e1 ++ " " ++ show e2
    show (EAbs e) = "(λ. " ++ show e ++ ")"

||| Capture-avoiding substitution.
total subst : (k : Nat) -> (v : Expr) -> (e : Expr) -> Expr
subst k1 v (EVar k2) = if k1 == k2 then v else EVar k2
subst k v (EAbs e) = EAbs (subst (S k) v e)
subst k v (EApp e1 e2) = EApp (subst k v e1) (subst k v e2)

||| Small step-semantics. Evaluate to Nothing if the expr is in normal form.
total step : Expr -> Maybe Expr
step (EVar _) = Nothing
step (EAbs x) = map EAbs (step x)
step (EApp (EAbs e1) e2) = Just $ subst 0 e2 e1
step (EApp e1 e2) =
    case (step e1) of
        Just e1' => Just $ EApp e1' e2
        Nothing => map (EApp e1) (step e2)

reduceLoop : Expr -> Expr
reduceLoop x =
    case (step x) of
        Just x' => reduceLoop x'
        Nothing => x

-- zero = \f. \x. x
zero : Expr
zero = EAbs (EAbs (EVar 0))

-- one = \f. \x. f x
one : Expr
one = EAbs (EAbs (EApp (EVar 1) (EVar 0)))

-- two = \f. \x. f (f x)
two : Expr
two = EAbs $ EAbs $ EApp (EVar 1) $ EApp (EVar 1) $ EVar 0

-- plus = \m. \n. \f. \x. m f (n f x)
plus : Expr
plus =
    let m = EVar 3 in
    let n = EVar 2 in
    let f = EVar 1 in
    let x = EVar 0 in
    let mf = EApp m f in
    let nfx = EApp (EApp n f) x in
    EAbs $ EAbs $ EAbs $ EAbs (EApp mf nfx)

-- (\x. x x) (\x. x x)
quine : Expr
quine = let xx = EAbs $ EApp (EVar 0) (EVar 0) in EApp xx xx

main : IO ()
main = do
    putStrLn $ "one = " ++ show one
    putStrLn $ "two = " ++ show two
    putStrLn $ "one + one = " ++ (show $ reduceLoop $ EApp (EApp plus one) one)
    putStrLn $ "two + two = " ++ (show $ reduceLoop $ EApp (EApp plus two) two)
    putStrLn $ "step quine = " ++ (show $ step quine)
