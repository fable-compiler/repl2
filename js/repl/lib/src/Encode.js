import { mapIndexed, map as map$$2, fold, iterate } from "../../fable-library/Seq.js";
import { empty, tryFind, toList } from "../../fable-library/Map.js";
import { toString as toString$$1 } from "../../fable-library/Date.js";
import { toString as toString$$2 } from "../../fable-library/TimeSpan.js";
import { toString as toString$$3 } from "../../fable-library/Long.js";
import { comparePrimitives, Lazy, mapCurriedArgs, uncurry } from "../../fable-library/Util.js";
import { defaultArg, defaultArgWith, map, some } from "../../fable-library/Option.js";
import { class_type, getGenerics, getGenericTypeDefinition, getTupleFields, getTupleElements, isTuple, isGenericType, getElementType, isArray, fullName, getUnionCaseFields, getUnionFields, isUnion, getRecordField, name, getRecordElements, isRecord } from "../../fable-library/Reflection.js";
import { fill, map as map$$1 } from "../../fable-library/Array.js";
import { toText, printf } from "../../fable-library/String.js";
import { declare } from "../../fable-library/Types.js";
import { Util$$$CachedEncoders as Util$0024$0024$0024CachedEncoders, Util$002ECache$00601$$GetOrAdd$$43981464 as Util$0024002ECache$002400601$0024$0024GetOrAdd$0024$002443981464 } from "./Types.js";
export function string(value) {
  return value;
}
export function guid(value$$1) {
  return value$$1;
}
export function int$(value$$2) {
  return value$$2;
}
export function uint32(value$$3) {
  return value$$3;
}
export function float$(value$$4) {
  return value$$4;
}
export function decimal(value$$5) {
  const value$$6 = String(value$$5);
  return value$$6;
}
export const nil = null;
export function bool(value$$8) {
  return value$$8;
}
export function object(values) {
  const o = {};
  iterate(function (forLoopVar) {
    o[forLoopVar[0]] = forLoopVar[1];
  }, values);
  return o;
}
export function array(values$$1) {
  return values$$1;
}
export function list(values$$2) {
  return Array.from(values$$2);
}
export function seq(values$$3) {
  return Array.from(values$$3);
}
export function dict(values$$4) {
  let values$$5;
  values$$5 = toList(values$$4);
  return object(values$$5);
}
export function bigint(value$$10) {
  return String(value$$10);
}
export function datetimeOffset(value$$11) {
  const value$$12 = toString$$1(value$$11, "O", {});
  return value$$12;
}
export function timespan(value$$14) {
  const value$$15 = toString$$2(value$$14);
  return value$$15;
}
export function int64(value$$17) {
  return String(value$$17);
}
export function uint64(value$$18) {
  return toString$$3(value$$18);
}
export function tuple2(enc1, enc2, v1, v2) {
  return [enc1(v1), enc2(v2)];
}
export function tuple3(enc1$$1, enc2$$1, enc3, v1$$1, v2$$1, v3) {
  return [enc1$$1(v1$$1), enc2$$1(v2$$1), enc3(v3)];
}
export function tuple4(enc1$$2, enc2$$2, enc3$$1, enc4, v1$$2, v2$$2, v3$$1, v4) {
  return [enc1$$2(v1$$2), enc2$$2(v2$$2), enc3$$1(v3$$1), enc4(v4)];
}
export function tuple5(enc1$$3, enc2$$3, enc3$$2, enc4$$1, enc5, v1$$3, v2$$3, v3$$2, v4$$1, v5) {
  return [enc1$$3(v1$$3), enc2$$3(v2$$3), enc3$$2(v3$$2), enc4$$1(v4$$1), enc5(v5)];
}
export function tuple6(enc1$$4, enc2$$4, enc3$$3, enc4$$2, enc5$$1, enc6, v1$$4, v2$$4, v3$$3, v4$$2, v5$$1, v6) {
  return [enc1$$4(v1$$4), enc2$$4(v2$$4), enc3$$3(v3$$3), enc4$$2(v4$$2), enc5$$1(v5$$1), enc6(v6)];
}
export function tuple7(enc1$$5, enc2$$5, enc3$$4, enc4$$3, enc5$$2, enc6$$1, enc7, v1$$5, v2$$5, v3$$4, v4$$3, v5$$2, v6$$1, v7) {
  return [enc1$$5(v1$$5), enc2$$5(v2$$5), enc3$$4(v3$$4), enc4$$3(v4$$3), enc5$$2(v5$$2), enc6$$1(v6$$1), enc7(v7)];
}
export function tuple8(enc1$$6, enc2$$6, enc3$$5, enc4$$4, enc5$$3, enc6$$2, enc7$$1, enc8, v1$$6, v2$$6, v3$$5, v4$$4, v5$$3, v6$$2, v7$$1, v8) {
  return [enc1$$6(v1$$6), enc2$$6(v2$$6), enc3$$5(v3$$5), enc4$$4(v4$$4), enc5$$3(v5$$3), enc6$$2(v6$$2), enc7$$1(v7$$1), enc8(v8)];
}
export function datetime(value$$19) {
  const value$$20 = toString$$1(value$$19, "O", {});
  return value$$20;
}
export function toString(space, value$$22) {
  return JSON.stringify(value$$22, uncurry(2, null), some(space));
}
export function option(encoder) {
  return function ($arg$$1) {
    let option$$2;
    option$$2 = map(encoder, $arg$$1);
    return defaultArgWith(option$$2, function defThunk() {
      return nil;
    });
  };
}
export function boxEncoder(d) {
  return d;
}
export function unboxEncoder(d$$1) {
  return d$$1;
}

function autoEncodeRecordsAndUnions(extra, isCamelCase, t) {
  if (isRecord(t, true)) {
    let setters;
    const array$$1 = getRecordElements(t, true);
    setters = map$$1(function mapping(fi) {
      const targetKey = isCamelCase ? name(fi).slice(undefined, 0 + 1).toLowerCase() + name(fi).slice(1, name(fi).length) : name(fi);
      const encode$$1 = autoEncoder(extra, isCamelCase, fi[1]);
      return function (source) {
        return function (target) {
          const value$$23 = getRecordField(source, fi);

          if (!(value$$23 == null)) {
            target[targetKey] = encode$$1(value$$23);
          } else {
            void null;
          }

          return target;
        };
      };
    }, array$$1, Array);
    return function (source$$1) {
      const state = {};
      return fold(uncurry(2, mapCurriedArgs(function folder(target$$1) {
        return function (set) {
          return set(source$$1, target$$1);
        };
      }, [0, [0, 2]])), state, setters);
    };
  } else if (isUnion(t, true)) {
    return function (value$$24) {
      const patternInput = getUnionFields(value$$24, t, true);
      const matchValue = patternInput[1].length | 0;

      if (matchValue === 0) {
        const value$$25 = name(patternInput[0]);
        return value$$25;
      } else {
        const fieldTypes = getUnionCaseFields(patternInput[0]);
        const target$$2 = fill(new Array(matchValue + 1), 0, matchValue + 1, null);
        const value$$26 = name(patternInput[0]);
        target$$2[0] = value$$26;

        for (let i = 1; i <= matchValue; i++) {
          const encode$$2 = autoEncoder(extra, isCamelCase, fieldTypes[i - 1][1]);
          target$$2[i] = encode$$2(patternInput[1][i - 1]);
        }

        return target$$2;
      }
    };
  } else {
    let message;
    const arg10 = fullName(t);
    const clo1 = toText(printf("Cannot generate auto encoder for %s. Please pass an extra encoder."));
    message = clo1(arg10);
    throw new Error(message);
  }
}

function autoEncoder(extra$$1, isCamelCase$$1, t$$1) {
  const fullname = fullName(t$$1);
  const matchValue$$1 = tryFind(fullname, extra$$1);

  if (matchValue$$1 == null) {
    if (isArray(t$$1)) {
      let encoder$$2;
      const t$$2 = getElementType(t$$1);
      encoder$$2 = autoEncoder(extra$$1, isCamelCase$$1, t$$2);
      return function (value$$27) {
        let values$$7;
        values$$7 = map$$2(encoder$$2, value$$27);
        return seq(values$$7);
      };
    } else if (isGenericType(t$$1)) {
      if (isTuple(t$$1)) {
        let encoders;
        const array$$2 = getTupleElements(t$$1);
        encoders = map$$1(function mapping$$1(t$$3) {
          return autoEncoder(extra$$1, isCamelCase$$1, t$$3);
        }, array$$2, Array);
        return function (value$$28) {
          let values$$8;
          const source$$4 = getTupleFields(value$$28);
          values$$8 = mapIndexed(function mapping$$2(i$$1, x) {
            return encoders[i$$1](x);
          }, source$$4);
          return seq(values$$8);
        };
      } else {
        const fullname$$1 = fullName(getGenericTypeDefinition(t$$1));

        if (fullname$$1 === "Microsoft.FSharp.Core.FSharpOption`1[System.Object]") {
          const encoder$$4 = new Lazy(function () {
            let d$$2;
            let encoder$$3;
            const t$$4 = getGenerics(t$$1)[0];
            encoder$$3 = autoEncoder(extra$$1, isCamelCase$$1, t$$4);
            d$$2 = option(encoder$$3);
            return d$$2;
          });
          return function d$$4(value$$29) {
            if (value$$29 == null) {
              return nil;
            } else {
              return encoder$$4.Value(value$$29);
            }
          };
        } else if (fullname$$1 === "Microsoft.FSharp.Collections.FSharpList`1[System.Object]" ? true : fullname$$1 === "Microsoft.FSharp.Collections.FSharpSet`1[System.Object]") {
          let encoder$$5;
          const t$$5 = getGenerics(t$$1)[0];
          encoder$$5 = autoEncoder(extra$$1, isCamelCase$$1, t$$5);
          return function (value$$30) {
            let values$$9;
            values$$9 = map$$2(encoder$$5, value$$30);
            return seq(values$$9);
          };
        } else if (fullname$$1 === "Microsoft.FSharp.Collections.FSharpMap`2[System.Object,System.Object]") {
          const keyType = getGenerics(t$$1)[0];
          let valueEncoder;
          const t$$6 = getGenerics(t$$1)[1];
          valueEncoder = autoEncoder(extra$$1, isCamelCase$$1, t$$6);

          if (fullName(keyType) === "System.String" ? true : fullName(keyType) === "System.Guid") {
            return function (value$$31) {
              const state$$1 = {};
              return fold(function folder$$1(target$$3, _arg1$$1) {
                const activePatternResult705 = _arg1$$1;
                target$$3[activePatternResult705[0]] = valueEncoder(activePatternResult705[1]);
                return target$$3;
              }, state$$1, value$$31);
            };
          } else {
            let keyEncoder;
            const clo3 = autoEncoder(extra$$1, isCamelCase$$1, keyType);

            keyEncoder = function (arg30) {
              return clo3(arg30);
            };

            return function (value$$32) {
              let values$$11;
              values$$11 = map$$2(function mapping$$3(_arg2) {
                const activePatternResult709 = _arg2;
                const values$$10 = [keyEncoder(activePatternResult709[0]), valueEncoder(activePatternResult709[1])];
                return values$$10;
              }, value$$32);
              return seq(values$$11);
            };
          }
        } else {
          return autoEncodeRecordsAndUnions(extra$$1, isCamelCase$$1, t$$1);
        }
      }
    } else if (fullname === "System.Boolean") {
      return function d$$5(value$$33) {
        return value$$33;
      };
    } else if (fullname === "System.String") {
      return function d$$6(value$$35) {
        return value$$35;
      };
    } else if (fullname === "System.Int32") {
      return function d$$7(value$$37) {
        return value$$37;
      };
    } else if (fullname === "System.UInt32") {
      return function d$$8(value$$39) {
        return value$$39;
      };
    } else if (fullname === "System.Double") {
      return function d$$9(value$$41) {
        return value$$41;
      };
    } else if (fullname === "System.DateTime") {
      return datetime;
    } else if (fullname === "System.DateTimeOffset") {
      return datetimeOffset;
    } else if (fullname === "System.TimeSpan") {
      return timespan;
    } else if (fullname === "System.Guid") {
      return guid;
    } else if (fullname === "System.Object") {
      return function d$$14(x$$1) {
        return x$$1;
      };
    } else {
      return autoEncodeRecordsAndUnions(extra$$1, isCamelCase$$1, t$$1);
    }
  } else {
    const encoder$$1 = matchValue$$1[0];
    return encoder$$1;
  }
}

export const Auto = declare(function Thoth_Json_Encode_Auto() {});
export function Auto$reflection() {
  return class_type("Thoth.Json.Encode.Auto", undefined, Auto);
}
export function Auto$$$generateEncoderCached$$4AE6C623(isCamelCase$$2, extra$$2, resolver) {
  const t$$8 = resolver.ResolveType();
  const d$$15 = Util$0024002ECache$002400601$0024$0024GetOrAdd$0024$002443981464(Util$0024$0024$0024CachedEncoders, fullName(t$$8), function () {
    const isCamelCase$$3 = defaultArg(isCamelCase$$2, false);
    let extra$$3;

    if (extra$$2 == null) {
      extra$$3 = empty({
        Compare: comparePrimitives
      });
    } else {
      const e = extra$$2;
      extra$$3 = e;
    }

    return autoEncoder(extra$$3, isCamelCase$$3, t$$8);
  });
  return d$$15;
}
export function Auto$$$generateEncoder$$4AE6C623(isCamelCase$$4, extra$$4, resolver$$2) {
  const isCamelCase$$5 = defaultArg(isCamelCase$$4, false);
  let extra$$5;

  if (extra$$4 == null) {
    extra$$5 = empty({
      Compare: comparePrimitives
    });
  } else {
    const e$$1 = extra$$4;
    extra$$5 = e$$1;
  }

  let d$$17;
  const t$$9 = resolver$$2.ResolveType();
  d$$17 = autoEncoder(extra$$5, isCamelCase$$5, t$$9);
  return d$$17;
}
export function Auto$$$toString$$59982D9A(space$$1, value$$47, isCamelCase$$6, extra$$6, resolver$$4) {
  const encoder$$6 = Auto$$$generateEncoder$$4AE6C623(isCamelCase$$6, extra$$6, resolver$$4);
  const value$$48 = encoder$$6(value$$47);
  return toString(space$$1, value$$48);
}
export function encode(space$$2, value$$49) {
  return toString(space$$2, value$$49);
}
