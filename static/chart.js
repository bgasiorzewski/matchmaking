"use strict";
var formatter = function (number) {
    return number + "%";
}
var g = new Dygraph(
    "chart",
    "http://www.ismatchmakingfixedyet.com/csv",
    {
        fillGraph: true,
        fractions: true,
        valueRange: [0, 100],
        axes: {
            y: { axisLabelFormatter: formatter }
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
