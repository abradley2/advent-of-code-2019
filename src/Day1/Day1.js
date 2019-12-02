import { Elm } from "./Day1.elm";
import PartOne from "./PartOne.txt";
import PartTwo from "./PartOne.txt";
import { mountSolution } from "../advent-utils";

mountSolution(Elm.Day1, 1, PartOne);
mountSolution(Elm.Day1, 2, PartTwo);



function page(state, action) {
  switch (action.type) {
    case 'ComponentMessage':
      const [nextState, extAction] = 
        Component.update(state.component, action.payload)
      switch (extAction.type) {
        case Component.ValueChanged:
          return {...nextState, value: extAction.value}
        default:
          nextState
      }
  }
}