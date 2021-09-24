/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 *
 * Inspired by https://github.com/eanbowman/sparkline.js
 */
function sparkline(_element) {
    const ctx = _element.getContext('2d');
    const data = _element.dataset.sparkline;
    const spark = data.split(',');
    for (var i in spark) {
        spark[i] = parseFloat(spark[i]);
    }

    const minValue = Math.min.apply(Math, spark);
    for (var j in spark) {
        spark[j] = spark[j] - minValue;
    }

    const margin = 2;
    const ratioW = ((_element.width - margin * 2) * 1) / spark.length;
    const ratioH = ((_element.height - margin * 2) * .8) / Math.max.apply(Math, spark);

    let x = 0;
    let y = 0;
    const grad = ctx.createLinearGradient(0, 0, _element.width, _element.height);
    grad.addColorStop(0, "#007AC9");
    grad.addColorStop(1, "#00c972");

    ctx.strokeStyle = grad;
    ctx.fillStyle = grad;

    ctx.beginPath();
    ctx.lineWidth = "1";
    ctx.arc(margin, _element.height - (spark[0] * ratioH + margin), 2, 0, 2 * Math.PI);
    ctx.fill();
    ctx.stroke();
    for (var index in spark) {
        if (index === 0) {
            ctx.beginPath();
            ctx.lineWidth = "1";
            ctx.moveTo(margin, _element.height - (spark[index] * ratioH + margin));
        } else {
            x = index * ratioW + margin;
            y = _element.height - (spark[index] * ratioH + margin);
            ctx.lineTo(x, y);
        }
    }
    ctx.stroke();

    ctx.beginPath();
    ctx.lineWidth = "1";
    ctx.arc(x, y, 2, 0, 2 * Math.PI);
    ctx.fill();
    ctx.stroke();
}

sirius.ready(function() {
    document.querySelectorAll('.sparkline-js').forEach(sparkline);
});