import { BigInteger$$$op_Inequality$$56F059C0 as BigInteger$0024$0024$0024op_Inequality$0024$002456F059C0, BigInteger$$$op_Equality$$56F059C0 as BigInteger$0024$0024$0024op_Equality$0024$002456F059C0, BigInteger$$$op_GreaterThanOrEqual$$56F059C0 as BigInteger$0024$0024$0024op_GreaterThanOrEqual$0024$002456F059C0, BigInteger$$$op_GreaterThan$$56F059C0 as BigInteger$0024$0024$0024op_GreaterThan$0024$002456F059C0, BigInteger$$$op_LessThanOrEqual$$56F059C0 as BigInteger$0024$0024$0024op_LessThanOrEqual$0024$002456F059C0, BigInteger$$$op_LessThan$$56F059C0 as BigInteger$0024$0024$0024op_LessThan$0024$002456F059C0, BigInteger$$$op_ExclusiveOr$$56F059C0 as BigInteger$0024$0024$0024op_ExclusiveOr$0024$002456F059C0, BigInteger$$$op_BitwiseOr$$56F059C0 as BigInteger$0024$0024$0024op_BitwiseOr$0024$002456F059C0, BigInteger$$$op_BitwiseAnd$$56F059C0 as BigInteger$0024$0024$0024op_BitwiseAnd$0024$002456F059C0, BigInteger$$$op_LeftShift$$62E082A2 as BigInteger$0024$0024$0024op_LeftShift$0024$002462E082A2, BigInteger$$$op_RightShift$$62E082A2 as BigInteger$0024$0024$0024op_RightShift$0024$002462E082A2, BigInteger$$$op_UnaryPlus$$Z665282C2 as BigInteger$0024$0024$0024op_UnaryPlus$0024$0024Z665282C2, BigInteger$$$op_UnaryNegation$$Z665282C2 as BigInteger$0024$0024$0024op_UnaryNegation$0024$0024Z665282C2, BigInteger$$$op_Modulus$$56F059C0 as BigInteger$0024$0024$0024op_Modulus$0024$002456F059C0, BigInteger$$$op_Division$$56F059C0 as BigInteger$0024$0024$0024op_Division$0024$002456F059C0, BigInteger$$$op_Multiply$$56F059C0 as BigInteger$0024$0024$0024op_Multiply$0024$002456F059C0, BigInteger$$$op_Subtraction$$56F059C0 as BigInteger$0024$0024$0024op_Subtraction$0024$002456F059C0, BigInteger$$$op_Addition$$56F059C0 as BigInteger$0024$0024$0024op_Addition$0024$002456F059C0, BigInteger$$get_IsOne as BigInteger$0024$0024get_IsOne, BigInteger$$get_IsZero as BigInteger$0024$0024get_IsZero, BigInteger$$get_Sign as BigInteger$0024$0024get_Sign, BigInteger$$get_ToDecimal as BigInteger$0024$0024get_ToDecimal, BigInteger$$get_ToDouble as BigInteger$0024$0024get_ToDouble, BigInteger$$get_ToSingle as BigInteger$0024$0024get_ToSingle, BigInteger$$get_ToUInt64 as BigInteger$0024$0024get_ToUInt64, BigInteger$$get_ToInt64 as BigInteger$0024$0024get_ToInt64, BigInteger$$get_ToUInt32 as BigInteger$0024$0024get_ToUInt32, BigInteger$$get_ToInt32 as BigInteger$0024$0024get_ToInt32, BigInteger$$get_ToUInt16 as BigInteger$0024$0024get_ToUInt16, BigInteger$$get_ToInt16 as BigInteger$0024$0024get_ToInt16, BigInteger$$get_ToByte as BigInteger$0024$0024get_ToByte, BigInteger$$get_ToSByte as BigInteger$0024$0024get_ToSByte, BigInteger$$$$002Ector$$Z524259A4 as BigInteger$0024$0024$0024$0024002Ector$0024$0024Z524259A4, BigInteger$$$$002Ector$$Z524259C1 as BigInteger$0024$0024$0024$0024002Ector$0024$0024Z524259C1, BigInteger$$$get_Two as BigInteger$0024$0024$0024get_Two, BigInteger$$$get_One as BigInteger$0024$0024$0024get_One, BigInteger$$$Abs$$Z665282C2 as BigInteger$0024$0024$0024Abs$0024$0024Z665282C2, BigInteger$$$Pow$$62E082A2 as BigInteger$0024$0024$0024Pow$0024$002462E082A2, BigInteger$$$GreatestCommonDivisor$$56F059C0 as BigInteger$0024$0024$0024GreatestCommonDivisor$0024$002456F059C0, BigInteger$$$DivRem$$56F059C0 as BigInteger$0024$0024$0024DivRem$0024$002456F059C0, BigInteger$$$Parse$$Z721C83C5 as BigInteger$0024$0024$0024Parse$0024$0024Z721C83C5, BigInteger$$$get_Zero as BigInteger$0024$0024$0024get_Zero, BigInteger } from "../BigInt/z.js";
import { fromInteger } from "./Long.js";
import { min, comparePrimitives, equals as equals$$1, structuralHash } from "./Util.js";
import { delay, unfold, rangeNumber } from "./Seq.js";
import { fold, head, skipWhile, find, ofSeq } from "./List.js";
import { List } from "./Types.js";
import { fill, reverse, ofList } from "./Array.js";
export function isBigInt(x) {
  return x instanceof BigInteger;
}
export function tryParse(str) {
  try {
    const res = BigInteger$0024$0024$0024Parse$0024$0024Z721C83C5(str);
    return [true, res];
  } catch (matchValue) {
    return [false, BigInteger$0024$0024$0024get_Zero()];
  }
}
export function parse(arg00) {
  return BigInteger$0024$0024$0024Parse$0024$0024Z721C83C5(arg00);
}
export function divRem(arg00$$1, arg01) {
  return BigInteger$0024$0024$0024DivRem$0024$002456F059C0(arg00$$1, arg01);
}
export function greatestCommonDivisor(arg00$$2, arg01$$1) {
  return BigInteger$0024$0024$0024GreatestCommonDivisor$0024$002456F059C0(arg00$$2, arg01$$1);
}
export function pow(arg00$$3, arg01$$2) {
  return BigInteger$0024$0024$0024Pow$0024$002462E082A2(arg00$$3, arg01$$2);
}
export function abs(arg00$$4) {
  return BigInteger$0024$0024$0024Abs$0024$0024Z665282C2(arg00$$4);
}
export const zero = BigInteger$0024$0024$0024get_Zero();
export const one = BigInteger$0024$0024$0024get_One();
export const two = BigInteger$0024$0024$0024get_Two();
export function fromString(s) {
  return BigInteger$0024$0024$0024Parse$0024$0024Z721C83C5(s);
}
export function fromZero() {
  return BigInteger$0024$0024$0024get_Zero();
}
export function fromOne() {
  return BigInteger$0024$0024$0024get_One();
}
export function fromInt64(i) {
  return BigInteger$0024$0024$0024$0024002Ector$0024$0024Z524259C1(i);
}
export function fromInt32(i$$1) {
  var value;

  if (i$$1 > 2147483647) {
    return BigInteger$0024$0024$0024$0024002Ector$0024$0024Z524259C1((value = i$$1, fromInteger(value, false, 6)));
  } else {
    return BigInteger$0024$0024$0024$0024002Ector$0024$0024Z524259A4(i$$1);
  }
}
export function toSByte(x$$1) {
  return BigInteger$0024$0024get_ToSByte(x$$1);
}
export function toByte(x$$2) {
  return BigInteger$0024$0024get_ToByte(x$$2);
}
export function toInt16(x$$3) {
  return BigInteger$0024$0024get_ToInt16(x$$3);
}
export function toUInt16(x$$4) {
  return BigInteger$0024$0024get_ToUInt16(x$$4);
}
export function toInt32(x$$5) {
  return BigInteger$0024$0024get_ToInt32(x$$5);
}
export function toUInt32(x$$6) {
  return BigInteger$0024$0024get_ToUInt32(x$$6);
}
export function toInt64(x$$7) {
  return BigInteger$0024$0024get_ToInt64(x$$7);
}
export function toUInt64(x$$8) {
  return BigInteger$0024$0024get_ToUInt64(x$$8);
}
export function toSingle(x$$9) {
  return BigInteger$0024$0024get_ToSingle(x$$9);
}
export function toDouble(x$$10) {
  return BigInteger$0024$0024get_ToDouble(x$$10);
}
export function toDecimal(x$$11) {
  return BigInteger$0024$0024get_ToDecimal(x$$11);
}
export function sign(x$$12) {
  return BigInteger$0024$0024get_Sign(x$$12);
}
export function isZero(x$$13) {
  return BigInteger$0024$0024get_IsZero(x$$13);
}
export function isOne(x$$14) {
  return BigInteger$0024$0024get_IsOne(x$$14);
}
export function hash(x$$15) {
  return structuralHash(x$$15);
}
export function compare(x$$16, y) {
  return x$$16.CompareTo(y);
}
export function equals(x$$17, y$$1) {
  return equals$$1(x$$17, y$$1);
}
export function toString(x$$18) {
  return String(x$$18);
}
export const get_Zero = BigInteger$0024$0024$0024get_Zero();
export const get_One = BigInteger$0024$0024$0024get_One();
export function op_Addition(arg00$$5, arg01$$3) {
  return BigInteger$0024$0024$0024op_Addition$0024$002456F059C0(arg00$$5, arg01$$3);
}
export function op_Subtraction(arg00$$6, arg01$$4) {
  return BigInteger$0024$0024$0024op_Subtraction$0024$002456F059C0(arg00$$6, arg01$$4);
}
export function op_Multiply(arg00$$7, arg01$$5) {
  return BigInteger$0024$0024$0024op_Multiply$0024$002456F059C0(arg00$$7, arg01$$5);
}
export function op_Division(arg00$$8, arg01$$6) {
  return BigInteger$0024$0024$0024op_Division$0024$002456F059C0(arg00$$8, arg01$$6);
}
export function op_Modulus(arg00$$9, arg01$$7) {
  return BigInteger$0024$0024$0024op_Modulus$0024$002456F059C0(arg00$$9, arg01$$7);
}
export function op_UnaryNegation(arg00$$10) {
  return BigInteger$0024$0024$0024op_UnaryNegation$0024$0024Z665282C2(arg00$$10);
}
export function op_UnaryPlus(arg00$$11) {
  return BigInteger$0024$0024$0024op_UnaryPlus$0024$0024Z665282C2(arg00$$11);
}
export function op_RightShift(arg00$$12, arg01$$8) {
  return BigInteger$0024$0024$0024op_RightShift$0024$002462E082A2(arg00$$12, arg01$$8);
}
export function op_LeftShift(arg00$$13, arg01$$9) {
  return BigInteger$0024$0024$0024op_LeftShift$0024$002462E082A2(arg00$$13, arg01$$9);
}
export function op_BitwiseAnd(arg00$$14, arg01$$10) {
  return BigInteger$0024$0024$0024op_BitwiseAnd$0024$002456F059C0(arg00$$14, arg01$$10);
}
export function op_BitwiseOr(arg00$$15, arg01$$11) {
  return BigInteger$0024$0024$0024op_BitwiseOr$0024$002456F059C0(arg00$$15, arg01$$11);
}
export function op_ExclusiveOr(arg00$$16, arg01$$12) {
  return BigInteger$0024$0024$0024op_ExclusiveOr$0024$002456F059C0(arg00$$16, arg01$$12);
}
export function op_LessThan(arg00$$17, arg01$$13) {
  return BigInteger$0024$0024$0024op_LessThan$0024$002456F059C0(arg00$$17, arg01$$13);
}
export function op_LessThanOrEqual(arg00$$18, arg01$$14) {
  return BigInteger$0024$0024$0024op_LessThanOrEqual$0024$002456F059C0(arg00$$18, arg01$$14);
}
export function op_GreaterThan(arg00$$19, arg01$$15) {
  return BigInteger$0024$0024$0024op_GreaterThan$0024$002456F059C0(arg00$$19, arg01$$15);
}
export function op_GreaterThanOrEqual(arg00$$20, arg01$$16) {
  return BigInteger$0024$0024$0024op_GreaterThanOrEqual$0024$002456F059C0(arg00$$20, arg01$$16);
}
export function op_Equality(arg00$$21, arg01$$17) {
  return BigInteger$0024$0024$0024op_Equality$0024$002456F059C0(arg00$$21, arg01$$17);
}
export function op_Inequality(arg00$$22, arg01$$18) {
  return BigInteger$0024$0024$0024op_Inequality$0024$002456F059C0(arg00$$22, arg01$$18);
}

function flipTwosComplement(currByte, lowBitFound) {
  if (lowBitFound) {
    return [(currByte ^ 255) & 255, true];
  } else if (currByte === 0) {
    return [0, false];
  } else {
    let firstBitIndex;
    const list = ofSeq(rangeNumber(0, 1, 7));
    firstBitIndex = find(function predicate(i$$2) {
      return (currByte & 1 << i$$2) > 0;
    }, list);
    return [(currByte ^ 254 << firstBitIndex) & 255, true];
  }
}

export function toByteArray(value$$1) {
  if (equals$$1(value$$1, zero)) {
    return new Uint8Array([0]);
  } else {
    const isPositive = value$$1.CompareTo(zero) > 0;
    const value$$2 = isPositive ? value$$1 : BigInteger$0024$0024$0024op_Multiply$0024$002456F059C0(BigInteger$0024$0024$0024$0024002Ector$0024$0024Z524259A4(-1), value$$1);
    let mask32;
    let i$$3;
    i$$3 = fromInteger(4294967295, false, 6);
    mask32 = fromInt64(i$$3);

    const loop = function loop($accumBytes$$74, $consumeValue$$75, $lowBitFound$$1$$76) {
      var value$$9, value$$10, value$$11;

      loop: while (true) {
        const accumBytes = $accumBytes$$74,
              consumeValue = $consumeValue$$75,
              lowBitFound$$1 = $lowBitFound$$1$$76;

        if (consumeValue.CompareTo(zero) <= 0) {
          let accumBytes$$1;

          if (isPositive) {
            accumBytes$$1 = skipWhile(function predicate$$1(b) {
              return b === 0;
            }, accumBytes);
          } else {
            accumBytes$$1 = skipWhile(function predicate$$2(b$$1) {
              return b$$1 === 255;
            }, accumBytes);
          }

          const isHighBitOne = (head(accumBytes$$1) & 128) !== 0;
          const accumBytes$$2 = (isPositive ? isHighBitOne : false) ? new List(0, accumBytes$$1) : (!isPositive ? !isHighBitOne : false) ? new List(255, accumBytes$$1) : accumBytes$$1;
          let array;
          array = ofList(accumBytes$$2, Uint8Array);
          return reverse(array, Uint8Array);
        } else {
          let currValue;
          const x$$19 = BigInteger$0024$0024$0024op_BitwiseAnd$0024$002456F059C0(consumeValue, mask32);
          currValue = toUInt32(x$$19);

          if (isPositive) {
            let b0;
            b0 = currValue & 0xFF;
            let b1;
            const value$$5 = currValue >>> 8;
            b1 = value$$5 & 0xFF;
            let b2;
            const value$$6 = currValue >>> 16;
            b2 = value$$6 & 0xFF;
            let b3;
            const value$$7 = currValue >>> 24;
            b3 = value$$7 & 0xFF;
            $accumBytes$$74 = new List(b3, new List(b2, new List(b1, new List(b0, accumBytes))));
            $consumeValue$$75 = BigInteger$0024$0024$0024op_RightShift$0024$002462E082A2(consumeValue, 32);
            $lowBitFound$$1$$76 = false;
            continue loop;
          } else {
            const patternInput = flipTwosComplement(currValue & 0xFF, lowBitFound$$1);
            const patternInput$$1 = flipTwosComplement((value$$9 = currValue >>> 8, value$$9 & 0xFF), patternInput[1]);
            const patternInput$$2 = flipTwosComplement((value$$10 = currValue >>> 16, value$$10 & 0xFF), patternInput$$1[1]);
            const patternInput$$3 = flipTwosComplement((value$$11 = currValue >>> 24, value$$11 & 0xFF), patternInput$$2[1]);
            $accumBytes$$74 = new List(patternInput$$3[0], new List(patternInput$$2[0], new List(patternInput$$1[0], new List(patternInput[0], accumBytes))));
            $consumeValue$$75 = BigInteger$0024$0024$0024op_RightShift$0024$002462E082A2(consumeValue, 32);
            $lowBitFound$$1$$76 = patternInput$$3[1];
            continue loop;
          }
        }

        break;
      }
    };

    return loop(new List(), value$$2, false);
  }
}
export function fromByteArray(bytes) {
  if (bytes == null) {
    throw new Error("bytes");
  } else {
    void null;
  }

  if (bytes.length === 0) {
    return zero;
  } else {
    const isPositive$$1 = (bytes[bytes.length - 1] & 128) === 0;
    const buffer = fill(new Uint8Array(4), 0, 4, 0);

    const loop$$1 = function loop$$1($accumUInt32$$80, $currIndex$$81, $bytesRemaining$$82, $lowBitFound$$6$$83) {
      loop$$1: while (true) {
        const accumUInt32 = $accumUInt32$$80,
              currIndex = $currIndex$$81,
              bytesRemaining = $bytesRemaining$$82,
              lowBitFound$$6 = $lowBitFound$$6$$83;

        if (bytesRemaining === 0) {
          let value$$14;
          value$$14 = fold(function folder(acc, value$$12) {
            var i$$4;
            return BigInteger$0024$0024$0024op_Addition$0024$002456F059C0(BigInteger$0024$0024$0024op_LeftShift$0024$002462E082A2(acc, 32), (i$$4 = fromInteger(value$$12, false, 6), fromInt64(i$$4)));
          }, zero, accumUInt32);

          if (isPositive$$1) {
            return value$$14;
          } else {
            return BigInteger$0024$0024$0024op_Multiply$0024$002456F059C0(BigInteger$0024$0024$0024$0024002Ector$0024$0024Z524259A4(-1), value$$14);
          }
        } else {
          const bytesToProcess = min(comparePrimitives, bytesRemaining, 4) | 0;

          for (let i$$5 = 0; i$$5 <= bytesToProcess - 1; i$$5++) {
            buffer[i$$5] = bytes[currIndex + i$$5];
          }

          if (isPositive$$1) {
            fill(buffer, bytesToProcess, 4 - bytesToProcess, 0);
            const value$$15 = (((buffer[0] | buffer[1] << 8 >>> 0) >>> 0 | buffer[2] << 16 >>> 0) >>> 0 | buffer[3] << 24 >>> 0) >>> 0;
            $accumUInt32$$80 = new List(value$$15, accumUInt32);
            $currIndex$$81 = currIndex + bytesToProcess;
            $bytesRemaining$$82 = bytesRemaining - bytesToProcess;
            $lowBitFound$$6$$83 = false;
            continue loop$$1;
          } else {
            fill(buffer, bytesToProcess, 4 - bytesToProcess, 255);
            const patternInput$$4 = flipTwosComplement(buffer[0], lowBitFound$$6);
            const patternInput$$5 = flipTwosComplement(buffer[1], patternInput$$4[1]);
            const patternInput$$6 = flipTwosComplement(buffer[2], patternInput$$5[1]);
            const patternInput$$7 = flipTwosComplement(buffer[3], patternInput$$6[1]);
            const value$$16 = (((patternInput$$4[0] | patternInput$$5[0] << 8 >>> 0) >>> 0 | patternInput$$6[0] << 16 >>> 0) >>> 0 | patternInput$$7[0] << 24 >>> 0) >>> 0;
            $accumUInt32$$80 = new List(value$$16, accumUInt32);
            $currIndex$$81 = currIndex + bytesToProcess;
            $bytesRemaining$$82 = bytesRemaining - bytesToProcess;
            $lowBitFound$$6$$83 = patternInput$$7[1];
            continue loop$$1;
          }
        }

        break;
      }
    };

    return loop$$1(new List(), 0, bytes.length, false);
  }
}
export function makeRangeStepFunction(step, last) {
  const stepComparedWithZero = step.CompareTo(zero) | 0;

  if (stepComparedWithZero === 0) {
    throw new Error("The step of a range cannot be zero");
  } else {
    void null;
  }

  const stepGreaterThanZero = stepComparedWithZero > 0;
  return function (x$$20) {
    const comparedWithLast = x$$20.CompareTo(last) | 0;
    return ((stepGreaterThanZero ? comparedWithLast <= 0 : false) ? true : !stepGreaterThanZero ? comparedWithLast >= 0 : false) ? [x$$20, BigInteger$0024$0024$0024op_Addition$0024$002456F059C0(x$$20, step)] : undefined;
  };
}
export function range(first, step$$1, last$$1) {
  const stepFn = makeRangeStepFunction(step$$1, last$$1);
  return delay(function () {
    return unfold(stepFn, first);
  });
}
