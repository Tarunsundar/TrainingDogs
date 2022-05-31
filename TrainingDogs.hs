module TrainingDogs where

import Data.List

type Dog = (Owner, Age, PaidTraining)

type Owner = String       -- name and contact details of the owner
type Age = Int            -- age of the dog in years
type PaidTraining = Bool  -- whether the fee has been paid

type Dogs = [Dog]

type Money = Float

oldDog :: Dog -> Bool
oldDog (o, a, p) = a > 12 

paidForTraining :: Dogs -> Dogs
paidForTraining [] = []
paidForTraining (d:ds) = 
  let (x,y,z) = d in
  if z then d:paidForTraining ds
       else paidForTraining ds
    
paidForTraining2 :: Dogs -> Dogs
paidForTraining2 ds =
    filter hasPaid ds
    where 
      hasPaid (o,a,p) = p    

income :: Dogs -> Money
income [] = 0
income ((o,a,p):ds) = 
  if p then 45 + income ds  
       else 30 + income ds
 

youngest :: Dogs -> Dogs
youngest [] = []
youngest ds =
   filter isYoungest ds 
   where
   ya = head (sort (map getAge ds))
   getAge :: Dog -> Age
   getAge (o,a,p) = a
   isYoungest :: Dog -> Bool
   isYoungest (o,a,p) = a == ya













