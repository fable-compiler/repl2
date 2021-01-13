export function tryParse(str) {
    // TODO: test if value is valid and in range
    if (str != null && /\S/.test(str)) {
        const v = +str.replace("_", "");
        if (!Number.isNaN(v)) {
            return [true, v];
        }
    }
    return [false, 0];
}
export function parse(str) {
    const [ok, value] = tryParse(str);
    if (ok) {
        return value;
    }
    else {
        throw new Error("Input string was not in a correct format.");
    }
}
// JS Number.isFinite function evals false for NaN
export function isInfinity(x) {
    return x === Number.POSITIVE_INFINITY || x === Number.NEGATIVE_INFINITY;
}
//# sourceMappingURL=Double.js.map