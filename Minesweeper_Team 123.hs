
-- Minesweeper Milestone 2

-- 4x4 Grid of Cells

-- Cell ( row , column )

type Cell = (Int,Int) 

-- S [Robot's position] [List of Postions of Mines] 
-- [The parent state is the last state the robot was in before doing the last performed action] 

data MyState = Null | S Cell [Cell] String MyState deriving (Show,Ord,Eq)


---------------------------------------------------------------
-- Higher Order Functions
-------------------------
-- Direction [ up , down , left , right ]

-- Type

direction :: String -> MyState -> MyState

-- Body

direction str (S (x,y) l ls state) | str == "up" && x == 0 = Null
								   | str == "down" && x == 3 = Null
								   | str == "left" && y == 0 = Null
								   | str == "right" && y == 3 = Null
							 
direction str (S (x,y) l ls state) | str == "up"   = S (x-1,y) l ("up")    (S (x,y) l ls state)
								   | str == "down" = S (x+1,y) l ("down")  (S (x,y) l ls state)
								   | str == "left" = S (x,y-1) l ("left")  (S (x,y) l ls state)
								   | str == "right"= S (x,y+1) l ("right") (S (x,y) l ls state)
								   
---------------------------
---------------------------------------------------------------
-- Helper Functions
-------------------
-- remove [remove element from a list]

-- Type

remove :: Eq a => a -> [a] -> [a]

-- Body

remove e (h:t) = [ x | x <- (h:t) , e /= x ]

---------------------------
-- searchH

-- Type

searchH :: [MyState] -> MyState

-- Body
searchH [] = Null
searchH ((S (x,y) l ls state):t) = if isGoal (S (x,y) l ls state) then (S (x,y) l ls state)
																  else searchH  t
---------------------------																  
---------------------------------------------------------------

-- UP

-- Type

up :: MyState -> MyState

-- Body

up state = direction "up" state
 
---------------------------

-- DOWN

-- Type

down :: MyState -> MyState

-- Body

down state = direction "down" state

---------------------------

-- LEFT

-- Type

left :: MyState -> MyState

-- Body

left state = direction "left" state

---------------------------

-- RIGHT

--Type

right :: MyState -> MyState

-- Body

right state = direction "right" state

---------------------------

-- COLLECT

-- Type

collect :: MyState -> MyState

-- Body

collect (S (x,y) l ls state) |  elem (x,y) l =  (S (x,y) (remove (x,y) l) "collect" (S (x,y) l ls state))
							 |  otherwise = Null

---------------------------

-- Next MyStates

-- Type 

nextMyStates:: MyState -> [MyState]

-- Body

nextMyStates state = [ x | x <- list , x /= Null]
			 where {
			 
				nextUp      = up    state;
				nextDown    = down  state;
				nextLeft    = left  state;
				nextRight   = right state;
				nextCollect = collect state;
				list        = [nextUp,nextDown,nextRight,nextLeft,nextCollect];
				
			 };

---------------------------

-- isGoal

-- Type

isGoal :: MyState -> Bool

-- Body

isGoal (S (x,y) l ls state) | l == [] = True
							| otherwise = False

---------------------------

-- Search

-- Type

search :: [MyState] -> MyState

-- Body

search [] = error "Empty List" -- PS : This must not happen

search (h:t) | isGoal h  = h
			 | find /= Null = find
			 | otherwise = search (t ++ (nextMyStates h))
		where{
		
			find = searchH t;
		
		}
			 
---------------------------

-- Construct Solution 

-- Type

constructSolution:: MyState ->[String]

-- Body

constructSolution (S (x,y) l ls state) | flag = constructSolution state ++ [ls]
									   | otherwise = []
				where {
					
					flag = ls == "collect" || ls == "up" || ls == "down" || ls == "left" || ls == "right";
				
				};

---------------------------

-- Solve 

-- Type

solve :: Cell -> [Cell] -> [String]

-- Body

solve cell (h:t) = solution
			where{
			
				initialState = (S cell (h:t) "" Null);
				goalState    = search [initialState];
				solution     = constructSolution goalState;
				
			};






