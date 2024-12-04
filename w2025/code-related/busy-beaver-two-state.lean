-- Two-State Busy Beaver Problem Solver
-- This implementation explores the maximum number of steps a 2-state Turing machine can run before halting

namespace BusyBeaver

-- Define the possible states
inductive State
  | A
  | B
  | Halt
  deriving Repr, Inhabited

-- Define the possible tape cell values
inductive Cell
  | Zero
  | One
  deriving Repr, Inhabited

-- Define the possible write actions
inductive WriteAction
  | WriteZero
  | WriteOne
  deriving Repr

-- Define the possible move directions
inductive MoveDirection
  | Left
  | Right
  deriving Repr

-- Represents a single transition rule
structure TransitionRule where
  currentState : State
  currentCell : Cell
  writeAction : WriteAction
  moveDirection : MoveDirection
  nextState : State
  deriving Repr

-- Tape representation with a focus on the current cell
structure Tape where
  left : List Cell   -- Cells to the left of the current cell
  current : Cell     -- Current cell
  right : List Cell  -- Cells to the right of the current cell
  deriving Repr

namespace Tape
  -- Create an empty tape
  def empty : Tape :=
    { left := [], current := .Zero, right := [] }

  -- Write a cell value to the current position
  def write (tape : Tape) (action : WriteAction) : Tape :=
    match action with
    | .WriteZero => { tape with current := .Zero }
    | .WriteOne  => { tape with current := .One }

  -- Move the tape head
  def move (tape : Tape) (direction : MoveDirection) : Tape :=
    match direction with
    | .Left  =>
        match tape.left with
        | []        => { left := [], current := .Zero, right := tape.current :: tape.right }
        | hd :: tl  => { left := tl, current := hd, right := tape.current :: tape.right }
    | .Right =>
        match tape.right with
        | []        => { left := tape.current :: tape.left, current := .Zero, right := [] }
        | hd :: tl  => { left := tape.current :: tape.left, current := hd, right := tl }
end Tape

-- Machine state representing the entire computation
structure MachineState where
  tape : Tape
  currentState : State
  stepCount : Nat
  deriving Repr

-- The transition function that defines the machine's behavior
def transitionFunction 
  (rules : List TransitionRule) 
  (state : MachineState) : Option MachineState :=
  -- Find the applicable rule based on current state and cell
  let applicableRule := rules.find? (fun rule =>
    rule.currentState = state.currentState ∧ 
    rule.currentCell = state.tape.current
  )
  
  -- Apply the rule if found
  match applicableRule with
  | some rule =>
    some {
      tape := (state.tape.write rule.writeAction).move rule.moveDirection,
      currentState := rule.nextState,
      stepCount := state.stepCount + 1
    }
  | none => none

-- Run the Turing machine to completion
partial def runMachine 
  (rules : List TransitionRule) 
  (initialState : MachineState) 
  (maxSteps : Nat) : MachineState :=
  let rec loop (currentState : MachineState) (remainingSteps : Nat) :=
    if remainingSteps = 0 then
      currentState
    else 
      match transitionFunction rules currentState with
      | some nextState => 
          if nextState.currentState = .Halt then
            nextState
          else
            loop nextState (remainingSteps - 1)
      | none => currentState
  loop initialState maxSteps

-- Generate all possible two-state transition rules
def generateAllRules : List TransitionRule :=
  let states := [State.A, State.B]
  let cells := [Cell.Zero, Cell.One]
  let writeActions := [WriteAction.WriteZero, WriteAction.WriteOne]
  let moveDirections := [MoveDirection.Left, MoveDirection.Right]
  let nextStates := states ++ [State.Halt]
  
  (states.bind fun currentState =>
    cells.bind fun currentCell =>
    writeActions.bind fun writeAction =>
    moveDirections.bind fun moveDirection =>
    nextStates.map fun nextState => 
    {
      currentState := currentState,
      currentCell := currentCell,
      writeAction := writeAction,
      moveDirection := moveDirection,
      nextState := nextState
    })

-- Find the busy beaver champion machine
def findBusyBeaverChampion (maxSteps : Nat) : List TransitionRule :=
  generateAllRules.foldl (fun champion rules =>
    let initialState : MachineState := {
      tape := Tape.empty,
      currentState := State.A,
      stepCount := 0
    }
    
    let finalState := runMachine rules initialState maxSteps
    
    -- Compare machines based on number of 1's written and steps taken
    let finalTape := finalState.tape
    let onesCount := 
      (finalTape.left.count (· = .One)) +
      (if finalTape.current = .One then 1 else 0) +
      (finalTape.right.count (· = .One))
    
    let currentChampionOnes := 
      let currentInitialState : MachineState := {
        tape := Tape.empty,
        currentState := State.A,
        stepCount := 0
      }
      let currentFinalState := runMachine champion currentInitialState maxSteps
      let currentTape := currentFinalState.tape
      (currentTape.left.count (· = .One)) +
      (if currentTape.current = .One then 1 else 0) +
      (currentTape.right.count (· = .One))
    
    if finalState.stepCount > currentChampionOnes then rules else champion
  ) []

-- Example usage and testing
def main : IO Unit := do
  let maxSteps := 100
  let championRules := findBusyBeaverChampion maxSteps
  IO.println s!"Busy Beaver Champion Rules: {championRules}"
  
  let initialState : MachineState := {
    tape := Tape.empty,
    currentState := State.A,
    stepCount := 0
  }
  
  let finalState := runMachine championRules initialState maxSteps
  IO.println s!"Final Machine State: {finalState}"
  IO.println s!"Steps Taken: {finalState.stepCount}"

end BusyBeaver
