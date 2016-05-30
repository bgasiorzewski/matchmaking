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
g.ready(function() {
    g.setAnnotations([
        {
            series: "mismatched games",
            x: "2016-05-17",
            shortText: "C",
            text: "Chromie patch"
        },
        {
            series: "mismatched games",
            x: "2016-05-24",
            shortText: "O",
            text: "Overwatch release"
        }
    ]);
});
