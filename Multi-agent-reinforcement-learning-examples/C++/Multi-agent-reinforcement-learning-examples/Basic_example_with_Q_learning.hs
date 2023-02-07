module Main where

import System.Random

numAgents = 2
numEpisodes = 100
numSteps = 100

data Agent = Agent { qTable :: [[Float]] }

initAgent :: Int -> Int -> Agent
initAgent stateCount actionCount = Agent (replicate stateCount (replicate actionCount 0.0))

chooseAction :: Agent -> Int -> IO Int
chooseAction agent state = do
  let qs = qTable agent !! state
  let maxQ = maximum qs
  let possibleActions = [i | i <- [0..(length qs - 1)], qs !! i == maxQ]
  let numPossibleActions = length possibleActions
  randomIndex <- randomRIO (0, numPossibleActions - 1)
  return $ possibleActions !! randomIndex

updateQTable :: Agent -> Int -> Int -> Int -> Float -> Agent
updateQTable agent state action nextState reward =
  let qs = qTable agent !! state
  let q = qs !! action
  let maxNextQ = maximum $ qTable agent !! nextState
  let newQ = q + 0.1 * (reward + 0.9 * maxNextQ - q)
  let newQs = take action qs ++ [newQ] ++ drop (action + 1) qs
  Agent (take state (qTable agent) ++ [newQs] ++ drop (state + 1) (qTable agent))

main :: IO ()
main = do
  let agents = replicate numAgents (initAgent 100 4)
  forM_ [0..(numEpisodes - 1)] $ \episode -> do
    forM_ [0..(numSteps - 1)] $ \step -> do
      forM_ [0..(numAgents - 1)] $ \i -> do
        state <- return 0 -- Get current state
        action <- chooseAction (agents !! i) state
        nextState <- return 0 -- Get next state based on state and action
        reward <- return 0.0 -- Get reward based on state and action
        let updatedAgent = updateQTable (agents !! i) state action nextState reward
        agents <- return $ take i agents ++ [updatedAgent] ++ drop (i + 1) agents
  return ()
