"use strict";
var formatter = function (precision) {
    return function (number) {
        return number.toFixed(precision) + "%";
    }
}
var g = new Dygraph(
    "chart",
    "http://www.ismatchmakingfixedyet.com/csv",
    {
        fillGraph: true,
        fractions: true,
        axes: {
            y: {
                axisLabelFormatter: formatter(0),
                valueFormatter: formatter(1)
            }
        }
    }
);
