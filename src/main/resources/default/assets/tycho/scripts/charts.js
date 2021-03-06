const SIRIUS_CHART_COLOR_WHEEL = [
    "#5cbae6",
    "#fac364",
    "#b6d957",
    "#8cd3ff",
    "#d998cb",
    "#98aafb"
];
const SIRIUS_INLINE_CHART_TYPES = [
    'bar', 'line', 'soft-bar', 'area'
];

let siriusChartNextColorWheelIndex = 0;
let siriusChartNextTypeIndex = 0;

/**
 * Uses the given string to randomize the color wheel and inline chart types.
 *
 * This way, we prevent, that all charts look exactly the same on each page.
 */
function randomizeCharts(randomizer) {
    for (let i = 0; i < randomizer.length; i++) {
        siriusChartNextColorWheelIndex += 13 * randomizer.charCodeAt(i);
        siriusChartNextTypeIndex += 13 * randomizer.charCodeAt(i);
    }
}

/**
 * Picks a random color from the list of chart colors.
 */
function pickFromColorWheel() {
    return SIRIUS_CHART_COLOR_WHEEL[siriusChartNextColorWheelIndex++ % SIRIUS_CHART_COLOR_WHEEL.length];
}

/**
 * Turns the given type into an inline chart type.
 *
 * Most notably, this will convert 'auto' into a randomly selected chart type.
 */
function makeInlineChartType(type) {
    if (type === 'auto' || sirius.isEmpty(type)) {
        return SIRIUS_INLINE_CHART_TYPES[siriusChartNextTypeIndex++ % SIRIUS_INLINE_CHART_TYPES.length];
    } else {
        return type;
    }
}

/**
 * Renders an inline chart.
 *
 * @param selector selects the target canvas
 * @param data provides the data to render
 * @param color specifies the color to use. Use null to pick a random color.
 * @param type specifies the chart type to use. Provide 'auto' to select a random type.
 */
function inlineChart(selector, data, color, type) {
    if (sirius.isEmpty(color)) {
        color = pickFromColorWheel();
    }
    type = makeInlineChartType(type);

    if (type === 'bar') {
        _inlineBarChart(selector, data, color, false);
    } else if (type === 'soft-bar') {
        _inlineBarChart(selector, data, color, true);
    } else if (type === 'area') {
        _inlineLineChart(selector, data, color, true);
    } else {
        _inlineLineChart(selector, data, color, false);
    }
}

function _inlineBarChart(selector, data, color, roundedBars) {
    let ctx = document.querySelector(selector);
    let myChart = new Chart(ctx, {
        type: 'bar',
        data: {
            labels: data,
            datasets: [{
                label: null,
                data: data,
                backgroundColor: color,
                borderRadius: roundedBars ? 4 : 0
            }]
        },
        options: {
            responsive: false,
            plugins: {
                tooltip: {
                    enabled: false
                },
                legend: {
                    display: false
                }
            },
            scales: {
                xAxis: {
                    display: false
                },
                yAxis: {
                    display: false
                },
            }
        }
    });
}

function _inlineLineChart(selector, data, color, fill) {
    let ctx = document.querySelector(selector);
    let myChart = new Chart(ctx, {
        type: 'line',
        data: {
            labels: data,
            datasets: [{
                label: null,
                data: data,
                borderColor: color,
                pointRadius: 0,
                fill: fill,
                backgroundColor: color
            }]
        },
        options: {
            responsive: false,
            plugins: {
                tooltip: {
                    enabled: false
                },
                legend: {
                    display: false
                }
            },
            scales: {
                xAxis: {
                    display: false
                },
                yAxis: {
                    display: false
                },
            }
        }
    });
}
