import { declare, Record } from "../../fable-library/Types.js";
import { record_type, string_type } from "../../fable-library/Reflection.js";
const Fragment$$1 = React.Fragment,
      PureComponent = React.PureComponent,
      Component = React.Component;
export const Component$00602 = Component;
export const PureComponent$00602 = PureComponent;
export const PureStatelessComponent$00601 = PureComponent;
export const FragmentProps = declare(function Fable_React_FragmentProps(key) {
  this.key = key;
}, Record);
export function FragmentProps$reflection() {
  return record_type("Fable.React.FragmentProps", [], FragmentProps, () => [["key", string_type]]);
}
export const Fragment = Fragment$$1;
