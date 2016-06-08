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
            x: "2016-05-09",
            shortText: "c",
            text: "Chromie PTR"
        },
        {
            series: "mismatched games",
            x: "2016-05-13",
            shortText: "T",
            text: "DreamHack Tours"
        },
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
        },
        {
            series: "mismatched games",
            x: "2016-06-03",
            shortText: "B",
            text: "ESL Burbank"
        },
        {
            series: "mismatched games",
            x: "2016-06-07",
            shortText: "m",
            text: "Medivh PTR"
        }
    ]);
});
