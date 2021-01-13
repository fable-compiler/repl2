import { declare, Record } from "../Types.js";
import { class_type } from "../Reflection.js";
import { BigNatModule$$$factorial as BigNatModule$0024$0024$0024factorial, BigNatModule$$$ofString as BigNatModule$0024$0024$0024ofString, BigNatModule$$$toFloat as BigNatModule$0024$0024$0024toFloat, BigNatModule$$$toUInt64 as BigNatModule$0024$0024$0024toUInt64, BigNatModule$$$toUInt32 as BigNatModule$0024$0024$0024toUInt32, BigNatModule$$$pow as BigNatModule$0024$0024$0024pow, BigNatModule$$$rem as BigNatModule$0024$0024$0024rem, BigNatModule$$$two as BigNatModule$0024$0024$0024two, BigNatModule$$$lte as BigNatModule$0024$0024$0024lte, BigNatModule$$$hcf as BigNatModule$0024$0024$0024hcf, BigNatModule$$$bitXor as BigNatModule$0024$0024$0024bitXor, BigNatModule$$$bitOr as BigNatModule$0024$0024$0024bitOr, BigNatModule$$$bitAnd as BigNatModule$0024$0024$0024bitAnd, BigNatModule$$$divmod as BigNatModule$0024$0024$0024divmod, BigNatModule$$$mul as BigNatModule$0024$0024$0024mul, BigNatModule$$$isOne as BigNatModule$0024$0024$0024isOne, BigNatModule$$$sub as BigNatModule$0024$0024$0024sub, BigNatModule$$$gte as BigNatModule$0024$0024$0024gte, BigNatModule$$$scale as BigNatModule$0024$0024$0024scale, BigNatModule$$$add as BigNatModule$0024$0024$0024add, BigNatModule$$$one as BigNatModule$0024$0024$0024one, BigNatModule$$$ofInt64 as BigNatModule$0024$0024$0024ofInt64, BigNatModule$$$toString as BigNatModule$0024$0024$0024toString, BigNatModule$$$hash as BigNatModule$0024$0024$0024hash, BigNatModule$$$gt as BigNatModule$0024$0024$0024gt, BigNatModule$$$lt as BigNatModule$0024$0024$0024lt, BigNatModule$$$isZero as BigNatModule$0024$0024$0024isZero, BigNatModule$$$equal as BigNatModule$0024$0024$0024equal, BigNatModule$$$getSmall as BigNatModule$0024$0024$0024getSmall, BigNatModule$$$isSmall as BigNatModule$0024$0024$0024isSmall, BigNatModule$$$ofInt32 as BigNatModule$0024$0024$0024ofInt32 } from "./n.js";
import { initialize } from "../Array.js";
import { op_Addition, op_Multiply, fromValue, equals, compare, fromBits, op_UnaryNegation, fromInteger } from "../Long.js";
import { op_UnaryNegation_Int32 } from "../Int32.js";
import Decimal from "../Decimal.js";
export const BigInteger = declare(function BigInt_BigInteger(signInt, v) {
  const $this$$1 = this;
  void null;
  $this$$1.signInt = signInt;
  $this$$1.v = v;
  void null;
}, Record);
export function BigInteger$reflection() {
  return class_type("BigInt.BigInteger", undefined, BigInteger);
}
export function BigInteger$$$$002Ector$$Z2BE94A1(signInt, v) {
  return this instanceof BigInteger ? BigInteger.call(this, signInt, v) : new BigInteger(signInt, v);
}

(function BigInteger$$$$002Ecctor() {
  BigInteger.smallLim = 4096;
  BigInteger.smallPosTab = initialize(BigInteger.smallLim, BigNatModule$0024$0024$0024ofInt32, Array);
  BigInteger.one = BigInteger$$$$002Ector$$Z524259A4(1);
  BigInteger.two = BigInteger$$$$002Ector$$Z524259A4(2);
  BigInteger.zero = BigInteger$$$$002Ector$$Z524259A4(0);
  void null;
})();

export function BigInteger$$$nat$$Z67CCE57D(n$$1) {
  if (BigNatModule$0024$0024$0024isSmall(n$$1) ? BigNatModule$0024$0024$0024getSmall(n$$1) < BigInteger.smallLim : false) {
    return BigInteger.smallPosTab[BigNatModule$0024$0024$0024getSmall(n$$1)];
  } else {
    return n$$1;
  }
}
export function BigInteger$$$create$$Z2BE94A1(s, n$$2) {
  return BigInteger$$$$002Ector$$Z2BE94A1(s, BigInteger$$$nat$$Z67CCE57D(n$$2));
}
export function BigInteger$$$posn$$Z67CCE57D(n$$3) {
  return BigInteger$$$$002Ector$$Z2BE94A1(1, BigInteger$$$nat$$Z67CCE57D(n$$3));
}
export function BigInteger$$$negn$$Z67CCE57D(n$$4) {
  return BigInteger$$$$002Ector$$Z2BE94A1(-1, BigInteger$$$nat$$Z67CCE57D(n$$4));
}
export function BigInteger$$get_Sign(x) {
  if (BigInteger$$get_IsZero(x)) {
    return 0;
  } else {
    return x.signInt | 0;
  }
}
export function BigInteger$$get_SignInt(x$$1) {
  return x$$1.signInt;
}
export function BigInteger$$get_V(x$$2) {
  return x$$2.v;
}
export function BigInteger$$$op_Equality$$56F059C0(x$$3, y) {
  const matchValue = [BigInteger$$get_SignInt(x$$3), BigInteger$$get_SignInt(y)];
  var $target$$12;

  if (matchValue[0] === -1) {
    if (matchValue[1] === -1) {
      $target$$12 = 1;
    } else if (matchValue[1] === 0) {
      $target$$12 = 8;
    } else if (matchValue[1] === 1) {
      $target$$12 = 3;
    } else {
      $target$$12 = 9;
    }
  } else if (matchValue[0] === 0) {
    if (matchValue[1] === -1) {
      $target$$12 = 6;
    } else if (matchValue[1] === 0) {
      $target$$12 = 4;
    } else if (matchValue[1] === 1) {
      $target$$12 = 5;
    } else {
      $target$$12 = 9;
    }
  } else if (matchValue[0] === 1) {
    if (matchValue[1] === -1) {
      $target$$12 = 2;
    } else if (matchValue[1] === 0) {
      $target$$12 = 7;
    } else if (matchValue[1] === 1) {
      $target$$12 = 0;
    } else {
      $target$$12 = 9;
    }
  } else {
    $target$$12 = 9;
  }

  switch ($target$$12) {
    case 0:
      {
        return BigNatModule$0024$0024$0024equal(BigInteger$$get_V(x$$3), BigInteger$$get_V(y));
      }

    case 1:
      {
        return BigNatModule$0024$0024$0024equal(BigInteger$$get_V(x$$3), BigInteger$$get_V(y));
      }

    case 2:
      {
        if (BigNatModule$0024$0024$0024isZero(BigInteger$$get_V(x$$3))) {
          return BigNatModule$0024$0024$0024isZero(BigInteger$$get_V(y));
        } else {
          return false;
        }
      }

    case 3:
      {
        if (BigNatModule$0024$0024$0024isZero(BigInteger$$get_V(x$$3))) {
          return BigNatModule$0024$0024$0024isZero(BigInteger$$get_V(y));
        } else {
          return false;
        }
      }

    case 4:
      {
        return true;
      }

    case 5:
      {
        return BigNatModule$0024$0024$0024isZero(BigInteger$$get_V(y));
      }

    case 6:
      {
        return BigNatModule$0024$0024$0024isZero(BigInteger$$get_V(y));
      }

    case 7:
      {
        return BigNatModule$0024$0024$0024isZero(BigInteger$$get_V(x$$3));
      }

    case 8:
      {
        return BigNatModule$0024$0024$0024isZero(BigInteger$$get_V(x$$3));
      }

    case 9:
      {
        throw new Error("signs should be +/- 1 or 0\\nParameter name: x");
      }
  }
}
export function BigInteger$$$op_Inequality$$56F059C0(x$$4, y$$1) {
  return !BigInteger$$$op_Equality$$56F059C0(x$$4, y$$1);
}
export function BigInteger$$$op_LessThan$$56F059C0(x$$5, y$$2) {
  const matchValue$$1 = [BigInteger$$get_SignInt(x$$5), BigInteger$$get_SignInt(y$$2)];
  var $target$$17;

  if (matchValue$$1[0] === -1) {
    if (matchValue$$1[1] === -1) {
      $target$$17 = 1;
    } else if (matchValue$$1[1] === 0) {
      $target$$17 = 8;
    } else if (matchValue$$1[1] === 1) {
      $target$$17 = 3;
    } else {
      $target$$17 = 9;
    }
  } else if (matchValue$$1[0] === 0) {
    if (matchValue$$1[1] === -1) {
      $target$$17 = 6;
    } else if (matchValue$$1[1] === 0) {
      $target$$17 = 4;
    } else if (matchValue$$1[1] === 1) {
      $target$$17 = 5;
    } else {
      $target$$17 = 9;
    }
  } else if (matchValue$$1[0] === 1) {
    if (matchValue$$1[1] === -1) {
      $target$$17 = 2;
    } else if (matchValue$$1[1] === 0) {
      $target$$17 = 7;
    } else if (matchValue$$1[1] === 1) {
      $target$$17 = 0;
    } else {
      $target$$17 = 9;
    }
  } else {
    $target$$17 = 9;
  }

  switch ($target$$17) {
    case 0:
      {
        return BigNatModule$0024$0024$0024lt(BigInteger$$get_V(x$$5), BigInteger$$get_V(y$$2));
      }

    case 1:
      {
        return BigNatModule$0024$0024$0024lt(BigInteger$$get_V(y$$2), BigInteger$$get_V(x$$5));
      }

    case 2:
      {
        return false;
      }

    case 3:
      {
        if (!BigNatModule$0024$0024$0024isZero(BigInteger$$get_V(x$$5))) {
          return true;
        } else {
          return !BigNatModule$0024$0024$0024isZero(BigInteger$$get_V(y$$2));
        }
      }

    case 4:
      {
        return false;
      }

    case 5:
      {
        return !BigNatModule$0024$0024$0024isZero(BigInteger$$get_V(y$$2));
      }

    case 6:
      {
        return false;
      }

    case 7:
      {
        return false;
      }

    case 8:
      {
        return !BigNatModule$0024$0024$0024isZero(BigInteger$$get_V(x$$5));
      }

    case 9:
      {
        throw new Error("signs should be +/- 1 or 0\\nParameter name: x");
      }
  }
}
export function BigInteger$$$op_GreaterThan$$56F059C0(x$$6, y$$3) {
  const matchValue$$2 = [BigInteger$$get_SignInt(x$$6), BigInteger$$get_SignInt(y$$3)];
  var $target$$20;

  if (matchValue$$2[0] === -1) {
    if (matchValue$$2[1] === -1) {
      $target$$20 = 1;
    } else if (matchValue$$2[1] === 0) {
      $target$$20 = 8;
    } else if (matchValue$$2[1] === 1) {
      $target$$20 = 3;
    } else {
      $target$$20 = 9;
    }
  } else if (matchValue$$2[0] === 0) {
    if (matchValue$$2[1] === -1) {
      $target$$20 = 6;
    } else if (matchValue$$2[1] === 0) {
      $target$$20 = 4;
    } else if (matchValue$$2[1] === 1) {
      $target$$20 = 5;
    } else {
      $target$$20 = 9;
    }
  } else if (matchValue$$2[0] === 1) {
    if (matchValue$$2[1] === -1) {
      $target$$20 = 2;
    } else if (matchValue$$2[1] === 0) {
      $target$$20 = 7;
    } else if (matchValue$$2[1] === 1) {
      $target$$20 = 0;
    } else {
      $target$$20 = 9;
    }
  } else {
    $target$$20 = 9;
  }

  switch ($target$$20) {
    case 0:
      {
        return BigNatModule$0024$0024$0024gt(BigInteger$$get_V(x$$6), BigInteger$$get_V(y$$3));
      }

    case 1:
      {
        return BigNatModule$0024$0024$0024gt(BigInteger$$get_V(y$$3), BigInteger$$get_V(x$$6));
      }

    case 2:
      {
        if (!BigNatModule$0024$0024$0024isZero(BigInteger$$get_V(x$$6))) {
          return true;
        } else {
          return !BigNatModule$0024$0024$0024isZero(BigInteger$$get_V(y$$3));
        }
      }

    case 3:
      {
        return false;
      }

    case 4:
      {
        return false;
      }

    case 5:
      {
        return false;
      }

    case 6:
      {
        return !BigNatModule$0024$0024$0024isZero(BigInteger$$get_V(y$$3));
      }

    case 7:
      {
        return !BigNatModule$0024$0024$0024isZero(BigInteger$$get_V(x$$6));
      }

    case 8:
      {
        return false;
      }

    case 9:
      {
        throw new Error("signs should be +/- 1 or 0\\nParameter name: x");
      }
  }
}
export function BigInteger$$$compare$$56F059C0(n$$5, nn) {
  if (BigInteger$$$op_LessThan$$56F059C0(n$$5, nn)) {
    return -1 | 0;
  } else if (BigInteger$$$op_Equality$$56F059C0(n$$5, nn)) {
    return 0;
  } else {
    return 1;
  }
}
export function BigInteger$$$hash$$Z665282C2(z) {
  if (BigInteger$$get_SignInt(z) === 0) {
    return 1;
  } else {
    return BigInteger$$get_SignInt(z) + BigNatModule$0024$0024$0024hash(BigInteger$$get_V(z)) | 0;
  }
}

BigInteger.prototype.toString = function () {
  const x$$7 = this;
  const matchValue$$3 = BigInteger$$get_SignInt(x$$7) | 0;

  switch (matchValue$$3) {
    case -1:
      {
        return BigNatModule$0024$0024$0024isZero(BigInteger$$get_V(x$$7)) ? "0" : "-" + BigNatModule$0024$0024$0024toString(BigInteger$$get_V(x$$7));
      }

    case 0:
      {
        return "0";
      }

    case 1:
      {
        return BigNatModule$0024$0024$0024toString(BigInteger$$get_V(x$$7));
      }

    default:
      {
        throw new Error("signs should be +/- 1 or 0");
      }
  }
};

export function BigInteger$$get_StructuredDisplayString(x$$8) {
  return String(x$$8);
}

BigInteger.prototype.Equals = function (obj) {
  const this$ = this;
  return obj instanceof BigInteger ? BigInteger$$$op_Equality$$56F059C0(this$, obj) : false;
};

BigInteger.prototype.GetHashCode = function () {
  const x$$9 = this;
  return BigInteger$$$hash$$Z665282C2(x$$9) | 0;
};

export function BigInteger$$$$002Ector$$Z524259A4(n$$6) {
  if (n$$6 >= 0) {
    return BigInteger$$$$002Ector$$Z2BE94A1.call(this, 1, BigInteger$$$nat$$Z67CCE57D(BigNatModule$0024$0024$0024ofInt32(n$$6)));
  } else if (n$$6 === -2147483648) {
    return BigInteger$$$$002Ector$$Z2BE94A1.call(this, -1, BigInteger$$$nat$$Z67CCE57D(BigNatModule$0024$0024$0024ofInt64(op_UnaryNegation(fromInteger(n$$6, false, 2)))));
  } else {
    return BigInteger$$$$002Ector$$Z2BE94A1.call(this, -1, BigInteger$$$nat$$Z67CCE57D(BigNatModule$0024$0024$0024ofInt32(op_UnaryNegation_Int32(n$$6))));
  }
}
export function BigInteger$$$$002Ector$$Z524259C1(n$$7) {
  if (compare(n$$7, fromBits(0, 0, false)) >= 0) {
    return BigInteger$$$$002Ector$$Z2BE94A1.call(this, 1, BigInteger$$$nat$$Z67CCE57D(BigNatModule$0024$0024$0024ofInt64(n$$7)));
  } else if (equals(n$$7, fromBits(0, 2147483648, false))) {
    return BigInteger$$$$002Ector$$Z2BE94A1.call(this, -1, BigInteger$$$nat$$Z67CCE57D(BigNatModule$0024$0024$0024add(BigNatModule$0024$0024$0024ofInt64(fromBits(4294967295, 2147483647, false)), BigNatModule$0024$0024$0024one)));
  } else {
    return BigInteger$$$$002Ector$$Z2BE94A1.call(this, -1, BigInteger$$$nat$$Z67CCE57D(BigNatModule$0024$0024$0024ofInt64(op_UnaryNegation(n$$7))));
  }
}
export function BigInteger$$$get_One() {
  return BigInteger.one;
}
export function BigInteger$$$get_Two() {
  return BigInteger.two;
}
export function BigInteger$$$get_Zero() {
  return BigInteger.zero;
}
export function BigInteger$$$op_UnaryNegation$$Z665282C2(z$$1) {
  const matchValue$$4 = BigInteger$$get_SignInt(z$$1) | 0;

  if (matchValue$$4 === 0) {
    return BigInteger$$$get_Zero();
  } else {
    return BigInteger$$$create$$Z2BE94A1(op_UnaryNegation_Int32(matchValue$$4), BigInteger$$get_V(z$$1));
  }
}
export function BigInteger$$$Scale$$Z320F31E(k, z$$2) {
  if (BigInteger$$get_SignInt(z$$2) === 0) {
    return BigInteger$$$get_Zero();
  } else if (k < 0) {
    return BigInteger$$$create$$Z2BE94A1(op_UnaryNegation_Int32(BigInteger$$get_SignInt(z$$2)), BigNatModule$0024$0024$0024scale(op_UnaryNegation_Int32(k), BigInteger$$get_V(z$$2)));
  } else {
    return BigInteger$$$create$$Z2BE94A1(BigInteger$$get_SignInt(z$$2), BigNatModule$0024$0024$0024scale(k, BigInteger$$get_V(z$$2)));
  }
}
export function BigInteger$$$subnn$$6A57060(nx, ny) {
  if (BigNatModule$0024$0024$0024gte(nx, ny)) {
    return BigInteger$$$posn$$Z67CCE57D(BigNatModule$0024$0024$0024sub(nx, ny));
  } else {
    return BigInteger$$$negn$$Z67CCE57D(BigNatModule$0024$0024$0024sub(ny, nx));
  }
}
export function BigInteger$$$addnn$$6A57060(nx$$1, ny$$1) {
  return BigInteger$$$posn$$Z67CCE57D(BigNatModule$0024$0024$0024add(nx$$1, ny$$1));
}
export function BigInteger$$get_IsZero(x$$10) {
  if (BigInteger$$get_SignInt(x$$10) === 0) {
    return true;
  } else {
    return BigNatModule$0024$0024$0024isZero(BigInteger$$get_V(x$$10));
  }
}
export function BigInteger$$get_IsOne(x$$11) {
  if (BigInteger$$get_SignInt(x$$11) === 1) {
    return BigNatModule$0024$0024$0024isOne(BigInteger$$get_V(x$$11));
  } else {
    return false;
  }
}
export function BigInteger$$$op_Addition$$56F059C0(x$$12, y$$4) {
  if (BigInteger$$get_IsZero(y$$4)) {
    return x$$12;
  } else if (BigInteger$$get_IsZero(x$$12)) {
    return y$$4;
  } else {
    const matchValue$$5 = [BigInteger$$get_SignInt(x$$12), BigInteger$$get_SignInt(y$$4)];
    var $target$$38;

    if (matchValue$$5[0] === -1) {
      if (matchValue$$5[1] === -1) {
        $target$$38 = 1;
      } else if (matchValue$$5[1] === 1) {
        $target$$38 = 3;
      } else {
        $target$$38 = 4;
      }
    } else if (matchValue$$5[0] === 1) {
      if (matchValue$$5[1] === -1) {
        $target$$38 = 2;
      } else if (matchValue$$5[1] === 1) {
        $target$$38 = 0;
      } else {
        $target$$38 = 4;
      }
    } else {
      $target$$38 = 4;
    }

    switch ($target$$38) {
      case 0:
        {
          return BigInteger$$$addnn$$6A57060(BigInteger$$get_V(x$$12), BigInteger$$get_V(y$$4));
        }

      case 1:
        {
          return BigInteger$$$op_UnaryNegation$$Z665282C2(BigInteger$$$addnn$$6A57060(BigInteger$$get_V(x$$12), BigInteger$$get_V(y$$4)));
        }

      case 2:
        {
          return BigInteger$$$subnn$$6A57060(BigInteger$$get_V(x$$12), BigInteger$$get_V(y$$4));
        }

      case 3:
        {
          return BigInteger$$$subnn$$6A57060(BigInteger$$get_V(y$$4), BigInteger$$get_V(x$$12));
        }

      case 4:
        {
          throw new Error("signs should be +/- 1\\nParameter name: x");
        }
    }
  }
}
export function BigInteger$$$op_Subtraction$$56F059C0(x$$13, y$$5) {
  if (BigInteger$$get_IsZero(y$$5)) {
    return x$$13;
  } else if (BigInteger$$get_IsZero(x$$13)) {
    return BigInteger$$$op_UnaryNegation$$Z665282C2(y$$5);
  } else {
    const matchValue$$6 = [BigInteger$$get_SignInt(x$$13), BigInteger$$get_SignInt(y$$5)];
    var $target$$41;

    if (matchValue$$6[0] === -1) {
      if (matchValue$$6[1] === -1) {
        $target$$41 = 1;
      } else if (matchValue$$6[1] === 1) {
        $target$$41 = 3;
      } else {
        $target$$41 = 4;
      }
    } else if (matchValue$$6[0] === 1) {
      if (matchValue$$6[1] === -1) {
        $target$$41 = 2;
      } else if (matchValue$$6[1] === 1) {
        $target$$41 = 0;
      } else {
        $target$$41 = 4;
      }
    } else {
      $target$$41 = 4;
    }

    switch ($target$$41) {
      case 0:
        {
          return BigInteger$$$subnn$$6A57060(BigInteger$$get_V(x$$13), BigInteger$$get_V(y$$5));
        }

      case 1:
        {
          return BigInteger$$$subnn$$6A57060(BigInteger$$get_V(y$$5), BigInteger$$get_V(x$$13));
        }

      case 2:
        {
          return BigInteger$$$addnn$$6A57060(BigInteger$$get_V(x$$13), BigInteger$$get_V(y$$5));
        }

      case 3:
        {
          return BigInteger$$$op_UnaryNegation$$Z665282C2(BigInteger$$$addnn$$6A57060(BigInteger$$get_V(x$$13), BigInteger$$get_V(y$$5)));
        }

      case 4:
        {
          throw new Error("signs should be +/- 1\\nParameter name: x");
        }
    }
  }
}
export function BigInteger$$$op_Multiply$$56F059C0(x$$14, y$$6) {
  if (BigInteger$$get_IsZero(x$$14)) {
    return x$$14;
  } else if (BigInteger$$get_IsZero(y$$6)) {
    return y$$6;
  } else if (BigInteger$$get_IsOne(x$$14)) {
    return y$$6;
  } else if (BigInteger$$get_IsOne(y$$6)) {
    return x$$14;
  } else {
    const m = BigNatModule$0024$0024$0024mul(BigInteger$$get_V(x$$14), BigInteger$$get_V(y$$6));
    return BigInteger$$$create$$Z2BE94A1(BigInteger$$get_SignInt(x$$14) * BigInteger$$get_SignInt(y$$6), m);
  }
}
export function BigInteger$$$DivRem$$56F059C0(x$$15, y$$7) {
  if (BigInteger$$get_IsZero(y$$7)) {
    throw new Error();
  } else {
    void null;
  }

  if (BigInteger$$get_IsZero(x$$15)) {
    return [BigInteger$$$get_Zero(), BigInteger$$$get_Zero()];
  } else {
    const patternInput = BigNatModule$0024$0024$0024divmod(BigInteger$$get_V(x$$15), BigInteger$$get_V(y$$7));
    const matchValue$$7 = [BigInteger$$get_SignInt(x$$15), BigInteger$$get_SignInt(y$$7)];
    var $target$$46;

    if (matchValue$$7[0] === -1) {
      if (matchValue$$7[1] === -1) {
        $target$$46 = 1;
      } else if (matchValue$$7[1] === 1) {
        $target$$46 = 3;
      } else {
        $target$$46 = 4;
      }
    } else if (matchValue$$7[0] === 1) {
      if (matchValue$$7[1] === -1) {
        $target$$46 = 2;
      } else if (matchValue$$7[1] === 1) {
        $target$$46 = 0;
      } else {
        $target$$46 = 4;
      }
    } else {
      $target$$46 = 4;
    }

    switch ($target$$46) {
      case 0:
        {
          return [BigInteger$$$posn$$Z67CCE57D(patternInput[0]), BigInteger$$$posn$$Z67CCE57D(patternInput[1])];
        }

      case 1:
        {
          return [BigInteger$$$posn$$Z67CCE57D(patternInput[0]), BigInteger$$$negn$$Z67CCE57D(patternInput[1])];
        }

      case 2:
        {
          return [BigInteger$$$negn$$Z67CCE57D(patternInput[0]), BigInteger$$$posn$$Z67CCE57D(patternInput[1])];
        }

      case 3:
        {
          return [BigInteger$$$negn$$Z67CCE57D(patternInput[0]), BigInteger$$$negn$$Z67CCE57D(patternInput[1])];
        }

      case 4:
        {
          throw new Error("signs should be +/- 1\\nParameter name: x");
        }
    }
  }
}
export function BigInteger$$$op_Division$$56F059C0(x$$16, y$$8) {
  const tuple = BigInteger$$$DivRem$$56F059C0(x$$16, y$$8);
  return tuple[0];
}
export function BigInteger$$$op_Modulus$$56F059C0(x$$17, y$$9) {
  const tuple$$1 = BigInteger$$$DivRem$$56F059C0(x$$17, y$$9);
  return tuple$$1[1];
}
export function BigInteger$$$op_RightShift$$62E082A2(x$$18, y$$10) {
  return BigInteger$$$op_Division$$56F059C0(x$$18, BigInteger$$$Pow$$62E082A2(BigInteger$$$get_Two(), y$$10));
}
export function BigInteger$$$op_LeftShift$$62E082A2(x$$19, y$$11) {
  return BigInteger$$$op_Multiply$$56F059C0(x$$19, BigInteger$$$Pow$$62E082A2(BigInteger$$$get_Two(), y$$11));
}
export function BigInteger$$$op_BitwiseAnd$$56F059C0(x$$20, y$$12) {
  return BigInteger$$$posn$$Z67CCE57D(BigNatModule$0024$0024$0024bitAnd(BigInteger$$get_V(x$$20), BigInteger$$get_V(y$$12)));
}
export function BigInteger$$$op_BitwiseOr$$56F059C0(x$$21, y$$13) {
  return BigInteger$$$posn$$Z67CCE57D(BigNatModule$0024$0024$0024bitOr(BigInteger$$get_V(x$$21), BigInteger$$get_V(y$$13)));
}
export function BigInteger$$$op_ExclusiveOr$$56F059C0(x$$22, y$$14) {
  return BigInteger$$$posn$$Z67CCE57D(BigNatModule$0024$0024$0024bitXor(BigInteger$$get_V(x$$22), BigInteger$$get_V(y$$14)));
}
export function BigInteger$$$GreatestCommonDivisor$$56F059C0(x$$23, y$$15) {
  const matchValue$$8 = [BigInteger$$get_SignInt(x$$23), BigInteger$$get_SignInt(y$$15)];

  if (matchValue$$8[0] === 0) {
    if (matchValue$$8[1] === 0) {
      return BigInteger$$$get_Zero();
    } else {
      return BigInteger$$$posn$$Z67CCE57D(BigInteger$$get_V(y$$15));
    }
  } else if (matchValue$$8[1] === 0) {
    return BigInteger$$$posn$$Z67CCE57D(BigInteger$$get_V(x$$23));
  } else {
    return BigInteger$$$posn$$Z67CCE57D(BigNatModule$0024$0024$0024hcf(BigInteger$$get_V(x$$23), BigInteger$$get_V(y$$15)));
  }
}
export function BigInteger$$get_IsNegative(x$$24) {
  if (BigInteger$$get_SignInt(x$$24) === -1) {
    return !BigInteger$$get_IsZero(x$$24);
  } else {
    return false;
  }
}
export function BigInteger$$get_IsPositive(x$$25) {
  if (BigInteger$$get_SignInt(x$$25) === 1) {
    return !BigInteger$$get_IsZero(x$$25);
  } else {
    return false;
  }
}
export function BigInteger$$$Abs$$Z665282C2(x$$26) {
  if (BigInteger$$get_SignInt(x$$26) === -1) {
    return BigInteger$$$op_UnaryNegation$$Z665282C2(x$$26);
  } else {
    return x$$26;
  }
}
export function BigInteger$$$op_LessThanOrEqual$$56F059C0(x$$27, y$$16) {
  const matchValue$$9 = [BigInteger$$get_SignInt(x$$27), BigInteger$$get_SignInt(y$$16)];
  var $target$$68;

  if (matchValue$$9[0] === -1) {
    if (matchValue$$9[1] === -1) {
      $target$$68 = 1;
    } else if (matchValue$$9[1] === 0) {
      $target$$68 = 6;
    } else if (matchValue$$9[1] === 1) {
      $target$$68 = 3;
    } else {
      $target$$68 = 9;
    }
  } else if (matchValue$$9[0] === 0) {
    if (matchValue$$9[1] === -1) {
      $target$$68 = 8;
    } else if (matchValue$$9[1] === 0) {
      $target$$68 = 4;
    } else if (matchValue$$9[1] === 1) {
      $target$$68 = 7;
    } else {
      $target$$68 = 9;
    }
  } else if (matchValue$$9[0] === 1) {
    if (matchValue$$9[1] === -1) {
      $target$$68 = 2;
    } else if (matchValue$$9[1] === 0) {
      $target$$68 = 5;
    } else if (matchValue$$9[1] === 1) {
      $target$$68 = 0;
    } else {
      $target$$68 = 9;
    }
  } else {
    $target$$68 = 9;
  }

  switch ($target$$68) {
    case 0:
      {
        return BigNatModule$0024$0024$0024lte(BigInteger$$get_V(x$$27), BigInteger$$get_V(y$$16));
      }

    case 1:
      {
        return BigNatModule$0024$0024$0024lte(BigInteger$$get_V(y$$16), BigInteger$$get_V(x$$27));
      }

    case 2:
      {
        if (BigNatModule$0024$0024$0024isZero(BigInteger$$get_V(x$$27))) {
          return BigNatModule$0024$0024$0024isZero(BigInteger$$get_V(y$$16));
        } else {
          return false;
        }
      }

    case 3:
      {
        return true;
      }

    case 4:
      {
        return true;
      }

    case 5:
      {
        return BigNatModule$0024$0024$0024isZero(BigInteger$$get_V(x$$27));
      }

    case 6:
      {
        return true;
      }

    case 7:
      {
        return true;
      }

    case 8:
      {
        return BigNatModule$0024$0024$0024isZero(BigInteger$$get_V(y$$16));
      }

    case 9:
      {
        throw new Error("signs should be +/- 1 or 0\\nParameter name: x");
      }
  }
}
export function BigInteger$$$op_GreaterThanOrEqual$$56F059C0(x$$28, y$$17) {
  const matchValue$$10 = [BigInteger$$get_SignInt(x$$28), BigInteger$$get_SignInt(y$$17)];
  var $target$$71;

  if (matchValue$$10[0] === -1) {
    if (matchValue$$10[1] === -1) {
      $target$$71 = 1;
    } else if (matchValue$$10[1] === 0) {
      $target$$71 = 6;
    } else if (matchValue$$10[1] === 1) {
      $target$$71 = 3;
    } else {
      $target$$71 = 9;
    }
  } else if (matchValue$$10[0] === 0) {
    if (matchValue$$10[1] === -1) {
      $target$$71 = 8;
    } else if (matchValue$$10[1] === 0) {
      $target$$71 = 4;
    } else if (matchValue$$10[1] === 1) {
      $target$$71 = 7;
    } else {
      $target$$71 = 9;
    }
  } else if (matchValue$$10[0] === 1) {
    if (matchValue$$10[1] === -1) {
      $target$$71 = 2;
    } else if (matchValue$$10[1] === 0) {
      $target$$71 = 5;
    } else if (matchValue$$10[1] === 1) {
      $target$$71 = 0;
    } else {
      $target$$71 = 9;
    }
  } else {
    $target$$71 = 9;
  }

  switch ($target$$71) {
    case 0:
      {
        return BigNatModule$0024$0024$0024gte(BigInteger$$get_V(x$$28), BigInteger$$get_V(y$$17));
      }

    case 1:
      {
        return BigNatModule$0024$0024$0024gte(BigInteger$$get_V(y$$17), BigInteger$$get_V(x$$28));
      }

    case 2:
      {
        return true;
      }

    case 3:
      {
        if (BigNatModule$0024$0024$0024isZero(BigInteger$$get_V(x$$28))) {
          return BigNatModule$0024$0024$0024isZero(BigInteger$$get_V(y$$17));
        } else {
          return false;
        }
      }

    case 4:
      {
        return true;
      }

    case 5:
      {
        return true;
      }

    case 6:
      {
        return BigNatModule$0024$0024$0024isZero(BigInteger$$get_V(x$$28));
      }

    case 7:
      {
        return BigNatModule$0024$0024$0024isZero(BigInteger$$get_V(y$$17));
      }

    case 8:
      {
        return true;
      }

    case 9:
      {
        throw new Error("signs should be +/- 1 or 0\\nParameter name: x");
      }
  }
}
export function BigInteger$$$Pow$$62E082A2(x$$29, y$$18) {
  if (y$$18 < 0) {
    throw new Error("y");
  } else {
    void null;
  }

  const matchValue$$11 = [BigInteger$$get_IsZero(x$$29), y$$18];

  if (matchValue$$11[0]) {
    if (matchValue$$11[1] === 0) {
      return BigInteger$$$get_One();
    } else {
      return BigInteger$$$get_Zero();
    }
  } else {
    const yval = BigInteger$$$$002Ector$$Z524259A4(y$$18);
    return BigInteger$$$create$$Z2BE94A1(BigNatModule$0024$0024$0024isZero(BigNatModule$0024$0024$0024rem(BigInteger$$get_V(yval), BigNatModule$0024$0024$0024two)) ? 1 : BigInteger$$get_SignInt(x$$29), BigNatModule$0024$0024$0024pow(BigInteger$$get_V(x$$29), BigInteger$$get_V(yval)));
  }
}
export function BigInteger$$get_ToInt32(x$$30) {
  if (BigInteger$$get_IsZero(x$$30)) {
    return 0;
  } else {
    const u = BigNatModule$0024$0024$0024toUInt32(BigInteger$$get_V(x$$30));

    if (u <= 2147483647 >>> 0) {
      return BigInteger$$get_SignInt(x$$30) * ~~u | 0;
    } else if (BigInteger$$get_SignInt(x$$30) === -1 ? u === 2147483647 + 1 >>> 0 : false) {
      return -2147483648 | 0;
    } else {
      throw new Error();
    }
  }
}
export function BigInteger$$get_ToUInt32(x$$31) {
  if (BigInteger$$get_IsZero(x$$31)) {
    return 0;
  } else {
    return BigNatModule$0024$0024$0024toUInt32(BigInteger$$get_V(x$$31));
  }
}
export function BigInteger$$get_ToInt64(x$$32) {
  if (BigInteger$$get_IsZero(x$$32)) {
    return fromBits(0, 0, false);
  } else {
    const u$$1 = BigNatModule$0024$0024$0024toUInt64(BigInteger$$get_V(x$$32));

    if (compare(u$$1, fromValue(fromBits(4294967295, 2147483647, false), true)) <= 0) {
      return op_Multiply(fromInteger(BigInteger$$get_SignInt(x$$32), false, 2), fromValue(u$$1, false));
    } else if (BigInteger$$get_SignInt(x$$32) === -1 ? equals(u$$1, fromValue(op_Addition(fromBits(4294967295, 2147483647, false), fromBits(1, 0, false)), true)) : false) {
      return fromBits(0, 2147483648, false);
    } else {
      throw new Error();
    }
  }
}
export function BigInteger$$get_ToUInt64(x$$33) {
  if (BigInteger$$get_IsZero(x$$33)) {
    return fromBits(0, 0, true);
  } else {
    return BigNatModule$0024$0024$0024toUInt64(BigInteger$$get_V(x$$33));
  }
}
export function BigInteger$$get_ToDouble(x$$34) {
  const matchValue$$12 = BigInteger$$get_SignInt(x$$34) | 0;

  switch (matchValue$$12) {
    case -1:
      {
        return -BigNatModule$0024$0024$0024toFloat(BigInteger$$get_V(x$$34));
      }

    case 0:
      {
        return 0;
      }

    case 1:
      {
        return BigNatModule$0024$0024$0024toFloat(BigInteger$$get_V(x$$34));
      }

    default:
      {
        throw new Error("signs should be +/- 1 or 0\\nParameter name: x");
      }
  }
}
export function BigInteger$$get_ToSByte(x$$35) {
  return (BigInteger$$get_ToInt32(x$$35) + 0x80 & 0xFF) - 0x80;
}
export function BigInteger$$get_ToByte(x$$36) {
  return BigInteger$$get_ToUInt32(x$$36) & 0xFF;
}
export function BigInteger$$get_ToInt16(x$$37) {
  return (BigInteger$$get_ToInt32(x$$37) + 0x8000 & 0xFFFF) - 0x8000;
}
export function BigInteger$$get_ToUInt16(x$$38) {
  return BigInteger$$get_ToUInt32(x$$38) & 0xFFFF;
}
export function BigInteger$$get_ToSingle(x$$39) {
  return BigInteger$$get_ToDouble(x$$39);
}
export function BigInteger$$get_ToDecimal(x$$40) {
  return new Decimal(BigInteger$$get_ToDouble(x$$40));
}
export function BigInteger$$$Parse$$Z721C83C5(text) {
  if (text == null) {
    throw new Error("text");
  } else {
    void null;
  }

  const text$$1 = text.trim();
  const len = text$$1.length | 0;

  if (len === 0) {
    throw new Error();
  } else {
    void null;
  }

  const matchValue$$13 = [text$$1[0], len];

  if (matchValue$$13[0] === "+") {
    if (matchValue$$13[1] === 1) {
      throw new Error();
    } else {
      return BigInteger$$$posn$$Z67CCE57D(BigNatModule$0024$0024$0024ofString(text$$1.slice(1, len - 1 + 1)));
    }
  } else if (matchValue$$13[0] === "-") {
    if (matchValue$$13[1] === 1) {
      throw new Error();
    } else {
      return BigInteger$$$negn$$Z67CCE57D(BigNatModule$0024$0024$0024ofString(text$$1.slice(1, len - 1 + 1)));
    }
  } else {
    return BigInteger$$$posn$$Z67CCE57D(BigNatModule$0024$0024$0024ofString(text$$1));
  }
}
export function BigInteger$$get_IsSmall(x$$41) {
  if (BigInteger$$get_IsZero(x$$41)) {
    return true;
  } else {
    return BigNatModule$0024$0024$0024isSmall(BigInteger$$get_V(x$$41));
  }
}
export function BigInteger$$$Factorial$$Z665282C2(x$$42) {
  if (BigInteger$$get_IsNegative(x$$42)) {
    throw new Error("mustBeNonNegative\\nParameter name: x");
  } else {
    void null;
  }

  if (BigInteger$$get_IsPositive(x$$42)) {
    return BigInteger$$$posn$$Z67CCE57D(BigNatModule$0024$0024$0024factorial(BigInteger$$get_V(x$$42)));
  } else {
    return BigInteger$$$get_One();
  }
}
export function BigInteger$$$op_UnaryPlus$$Z665282C2(n1) {
  return n1;
}
export function BigInteger$$$FromInt64$$Z524259C1(x$$43) {
  return BigInteger$$$$002Ector$$Z524259C1(x$$43);
}
export function BigInteger$$$FromInt32$$Z524259A4(x$$44) {
  return BigInteger$$$$002Ector$$Z524259A4(x$$44);
}

BigInteger.prototype.CompareTo = function (obj$$1) {
  const this$$$1 = this;

  if (obj$$1 instanceof BigInteger) {
    return BigInteger$$$compare$$56F059C0(this$$$1, obj$$1) | 0;
  } else {
    throw new Error("the objects are not comparable\\nParameter name: obj");
  }
};