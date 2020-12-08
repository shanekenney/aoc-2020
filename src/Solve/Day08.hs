module Solve.Day08 where

import Data.Function ((&))
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import Data.String.Utils
import Data.Vector (Vector, (!), (//))
import qualified Data.Vector as Vector

input :: String
input = "acc +15\njmp +461\nacc +6\nnop +445\njmp +324\njmp +253\nacc -4\nacc +22\nacc +11\njmp +471\njmp +145\nacc +19\njmp -7\njmp +431\nnop +66\nacc +48\njmp +409\njmp +514\njmp +1\nacc +32\njmp +552\nacc +21\njmp +317\nnop +488\njmp +500\njmp +214\nacc +41\njmp +17\nacc +19\njmp +1\nacc +28\njmp +74\nacc +37\nacc +46\nacc -10\njmp +455\nacc +33\njmp +585\nacc -13\nacc +18\njmp +19\njmp +601\nacc +30\njmp +272\nacc +18\nacc +50\nacc +1\nacc +29\njmp +50\nnop +573\njmp +562\nnop +274\njmp +1\nacc +6\njmp +1\njmp +64\njmp +1\nacc +37\njmp +161\nnop +549\nacc +21\njmp +1\nnop +325\njmp +331\nacc +0\nacc +25\njmp +431\nnop +349\njmp +45\nacc +37\nacc +9\njmp +354\njmp +132\njmp +307\njmp +1\nnop +465\njmp +386\nacc +13\nacc +20\njmp -20\nacc -15\nnop +158\njmp +415\nacc +33\nacc +40\nacc +38\nacc -2\njmp +81\nacc +44\nacc +9\nacc +15\nnop +139\njmp -18\nnop +187\nacc +47\nacc +45\nacc +40\njmp +456\nacc +48\njmp +1\njmp -75\nacc +11\nacc -13\nnop +222\njmp +202\nacc +38\nacc +31\nacc -11\nacc +1\njmp +431\nacc +37\nnop +195\njmp +118\nacc -8\njmp +1\njmp +154\nacc +7\nacc +13\njmp +43\njmp +507\nacc -2\nacc +11\njmp +465\nacc -12\nnop -103\njmp +50\nacc +2\nnop +4\nacc +46\nacc +17\njmp +100\nnop +149\njmp +397\nnop -28\njmp +236\nacc +0\nnop +465\nacc +19\nacc -5\njmp +91\njmp +118\nacc -3\nnop +176\nacc +8\njmp +24\njmp +235\nacc -7\nnop -1\nnop +347\nacc +33\njmp +165\nacc -18\nacc +9\nacc -8\njmp +179\nacc -3\nacc +40\njmp +1\nacc +5\njmp +475\njmp +70\nacc +50\nacc -1\nacc -11\nacc +43\njmp +283\nnop +114\njmp +161\nacc -19\nacc +50\nacc +47\nacc +49\njmp +381\njmp +1\njmp -94\nnop +373\nacc +26\nacc +7\njmp +41\nacc -7\njmp +264\nnop -40\nacc +23\njmp -60\nacc +15\nacc +19\nacc +39\nacc -16\njmp +283\nacc +4\njmp +258\nacc +0\nacc +38\nacc -6\njmp +32\nacc +33\njmp +1\nacc -4\nacc +17\njmp +149\nacc -17\nacc +39\nnop +75\njmp -136\nacc +6\nacc +10\njmp +1\nacc +9\njmp +390\njmp +363\nacc +32\njmp +95\njmp -71\nacc +12\njmp +86\nacc +49\nacc +2\nacc +0\njmp +139\nnop +363\nacc +21\njmp +366\njmp +1\nacc +16\nacc -13\njmp +55\njmp -159\nacc +46\nacc +50\njmp +266\nacc +2\njmp -180\njmp -181\nacc +41\nacc -3\nnop -176\njmp +121\nacc +23\njmp +281\nacc +5\nacc +14\nacc -8\njmp +128\njmp +281\nnop +81\njmp +1\nacc +36\nacc +34\njmp -176\nacc +8\njmp +309\nacc +11\nnop -77\njmp -40\nacc +0\nacc +8\nacc +14\njmp +296\nnop +197\nnop -124\nacc +38\nacc +9\njmp +310\njmp -183\njmp +353\nacc +28\nacc -18\nnop +120\njmp -217\nacc +49\nacc +38\njmp -243\nnop -68\nacc +9\nacc +15\nnop +43\njmp -46\nacc -4\njmp +262\njmp +176\nnop -139\nacc +38\nacc +35\nacc -8\njmp -225\nacc +37\nacc +49\nacc +42\nnop +278\njmp +264\nacc -11\nnop +291\nacc +21\nnop -221\njmp +80\nacc +15\nacc +13\nacc +2\nnop -40\njmp +309\nacc -18\nacc -5\nacc +24\njmp -70\nacc +12\njmp -261\nacc +4\nacc -2\nacc +3\njmp +1\njmp +302\nacc -7\njmp +1\nacc +39\njmp +73\njmp +18\nacc +3\njmp +277\nacc +4\nnop +125\nnop -284\nacc +41\njmp -312\nacc +13\njmp -183\nnop -35\njmp -137\njmp -76\nacc +1\nacc +31\nacc +39\njmp +56\njmp +290\njmp +1\nacc +12\nnop +71\nacc +43\njmp -296\njmp +68\nacc -14\nacc +35\nnop -290\njmp -24\nacc +39\nacc -8\nnop +110\njmp +1\njmp +78\nacc +11\nacc +9\nacc -11\nacc +50\njmp +167\nacc +50\nacc +19\nacc +0\njmp -221\nacc -4\nacc -6\nnop +11\nacc +49\njmp -348\nnop +197\nacc +6\njmp -196\nnop -155\njmp -76\nacc +50\nacc -8\njmp -89\nacc +9\njmp +255\nacc +25\njmp +199\njmp -35\nacc +46\nacc +25\njmp +1\nacc +32\njmp -31\nacc -3\nacc -5\njmp +73\nacc +7\nacc +22\nacc -15\njmp +145\nnop -97\nacc +47\nacc +22\njmp -110\nacc +44\nacc +4\njmp -383\nacc +34\njmp +1\njmp +16\nnop -128\nacc +43\njmp -34\nnop +95\nacc +3\njmp +4\nacc +13\nacc -2\njmp -90\nacc +39\njmp +187\nacc +24\nacc +23\nacc +42\njmp -11\njmp -281\njmp +1\nacc +25\njmp -157\nacc +3\nacc -3\nnop -24\nacc -13\njmp -46\nacc +10\nacc +16\nnop -7\njmp -289\nnop -408\nacc -5\nacc +23\nnop +91\njmp -234\nacc +0\nacc +4\nacc +15\nacc -15\njmp -367\nacc +32\nacc -9\nacc +13\njmp -194\nacc +38\nnop +126\nacc +1\nnop +124\njmp -275\nacc -14\nacc +26\njmp +55\njmp -388\nacc +3\nacc +8\nacc +31\nacc +34\njmp -372\nacc +45\njmp -115\nacc -6\nacc +47\nacc -17\nacc +29\njmp -438\nacc +33\njmp -113\njmp -301\njmp -396\nacc +46\njmp -284\nacc -14\nacc +11\nacc +20\nnop -356\njmp -445\nacc +20\nacc -6\nacc -8\njmp +134\nnop +54\nacc +33\njmp +1\nacc +3\njmp +108\nacc +14\nnop +67\nnop -66\nacc +45\njmp +117\nacc +45\nacc +42\nacc +25\nacc -18\njmp -354\nacc +8\njmp -240\nnop -373\nacc -8\njmp +72\njmp -95\njmp -350\njmp -62\nacc +6\nacc -18\njmp +108\nacc +14\nacc +11\nnop -164\nacc +4\njmp +6\nacc +24\nacc +11\nacc -7\nacc +27\njmp -171\nacc +23\nacc +36\nacc +20\nacc +42\njmp -51\nacc +28\nacc +10\njmp -218\nnop +63\njmp -294\nacc -10\nnop +44\njmp +43\njmp -444\nacc +20\nacc +39\nacc +29\njmp -507\njmp -265\njmp -471\nnop +17\nacc +39\nacc +4\njmp -54\nacc +1\nnop -448\nacc -18\nacc +3\njmp -495\nacc +17\nacc +16\njmp +6\nacc +6\nacc +0\njmp +1\nacc -15\njmp -317\njmp -77\nacc +4\nacc +30\nacc -3\njmp -187\nacc -11\nnop -189\nnop -488\njmp -140\nacc +50\njmp -142\nnop -211\njmp -166\nacc -12\nacc +7\nacc +32\nacc +40\njmp -384\njmp -186\nnop -261\nacc +32\nacc +19\nacc +44\njmp +16\nacc +15\nacc +30\nnop -476\nacc +9\njmp -299\nacc -17\nacc -17\nacc -4\nacc +44\njmp -133\njmp -58\nacc +21\nacc +4\nacc -19\njmp -170\nacc +32\nacc -3\njmp -363\nacc +48\nacc +9\nacc +48\njmp +5\nacc +30\nacc +40\njmp -450\njmp -282\njmp -388\nacc +12\njmp -361\nacc -6\njmp -237\nacc +27\nacc +16\nacc -19\nacc -5\njmp -145\nacc +38\njmp -565\nnop -341\njmp +11\nacc +22\nnop -219\njmp -597\nacc +33\njmp -572\njmp -292\nacc +7\nacc -14\nacc +33\njmp -432\nacc +47\njmp -41\nnop -306\njmp -85\njmp +1\nnop -215\nacc +30\nacc +9\njmp -71\nacc +42\nacc +49\njmp -553\nacc +28\nacc +43\njmp +1\njmp -147\nacc +44\nacc +26\nnop -176\njmp -582\nacc +7\nacc -14\nacc +16\nacc +34\njmp +1"

data Instruction
  = Acc Int
  | Jmp Int
  | Nop Int
  deriving (Show)

toInstruction :: String -> Instruction
toInstruction str =
  case op of
    "acc" -> Acc intVal
    "jmp" -> Jmp intVal
    "nop" -> Nop intVal
    _ -> error "Invalid instruction"
  where
    [op, val] = words str
    intVal = read $ replace "+" "" val

isAcc :: Instruction -> Bool
isAcc = \case
  Acc _ -> True
  _ -> False

toggleInstruction :: Instruction -> Instruction
toggleInstruction = \case
  Jmp val -> Nop val
  Nop val -> Jmp val
  Acc val -> Acc val

readInstructions :: String -> [Instruction]
readInstructions value =
  toInstruction <$> lines value

data ConsoleState = ConsoleState
  { accumlator :: Int,
    programCounter :: Int,
    instructions :: Vector Instruction,
    executed :: IntSet,
    previousInstructions :: [(Int, Instruction)]
  }
  deriving (Show)

initConsole :: [Instruction] -> ConsoleState
initConsole instructions =
  ConsoleState
    { accumlator = 0,
      programCounter = 0,
      instructions = Vector.fromList instructions,
      executed = IntSet.empty,
      previousInstructions = []
    }

resetConsole :: ConsoleState -> ConsoleState
resetConsole ConsoleState {instructions} =
  initConsole $ Vector.toList instructions

toggleInstructionAt :: (Int, Instruction) -> ConsoleState -> ConsoleState
toggleInstructionAt (location, instruction) state@ConsoleState {instructions} =
  state {instructions = instructions // [(location, toggleInstruction instruction)]}

isTerminated :: ConsoleState -> Bool
isTerminated ConsoleState {programCounter, instructions} =
  programCounter >= Vector.length instructions

execute :: ConsoleState -> ConsoleState
execute state@ConsoleState {accumlator, programCounter, instructions, executed, previousInstructions} =
  let instruction = instructions ! programCounter
      isExecuted = IntSet.member programCounter executed
      nextState =
        state
          { executed = IntSet.insert programCounter executed,
            previousInstructions = (programCounter, instruction) : previousInstructions
          }
   in if not isExecuted && not (isTerminated state)
        then case instruction of
          Acc value ->
            execute
              nextState
                { programCounter = programCounter + 1,
                  accumlator = accumlator + value
                }
          Jmp value ->
            execute
              nextState
                { programCounter = programCounter + value
                }
          Nop _ ->
            execute
              nextState
                { programCounter = programCounter + 1
                }
        else state

checkConsole :: [(Int, Instruction)] -> ConsoleState -> ConsoleState
checkConsole instructions console =
  if not $ isTerminated console
    then
      let (lastInstruction : instructions') = instructions
       in checkConsole
            instructions'
            ( console
                & toggleInstructionAt lastInstruction
                & resetConsole
                & execute
            )
    else console

partOne :: String -> Int
partOne value = accumlator $ execute console
  where
    console = initConsole $ readInstructions value

partTwo :: String -> Int
partTwo value =
  accumlator $ checkConsole candidates executed
  where
    executed = execute $ initConsole $ readInstructions value
    candidates = filter (not . isAcc . snd) $ previousInstructions executed
