/*
 * Made with all the love in the world
 * by scireum in Remshalden, Germany
 *
 * Copyright by scireum GmbH
 * http://www.scireum.de - info@scireum.de
 *
 * Inspired by https://github.com/eanbowman/sparkline.js
 */
function sparkline(element) {
    const ctx = element.getContext("2d");
    const data = element.getAttribute("data-sparkline");
    const spark = data.split(',');
    for (const i in spark) {
        spark[i] = parseFloat(spark[i]);
    }

    const minValue = Math.min.apply(Math, spark);
    for (const j in spark) {
        spark[j] = spark[j] - minValue;
    }

    const margin = 2;
    const ratioW = ((element.width - margin * 2) * 1) / spark.length;
    const ratioH = ((element.height - margin * 2) * .8) / Math.max.apply(Math, spark);

    let x = 0;
    let y = 0;
    const grad = ctx.createLinearGradient(0, 0, element.width, element.height);
    grad.addColorStop(0, "#007AC9");
    grad.addColorStop(1, "#00c972");

    ctx.strokeStyle = grad;
    ctx.fillStyle = grad;

    ctx.beginPath();
    ctx.lineWidth = "1";
    ctx.arc(margin, element.height - (spark[0] * ratioH + margin), 2, 0, 2 * Math.PI);
    ctx.fill();
    ctx.stroke();
    for (const index in spark) {
        if (index === 0) {
            ctx.beginPath();
            ctx.lineWidth = "1";
            ctx.moveTo(margin, element.height - (spark[index] * ratioH + margin));
        } else {
            x = index * ratioW + margin;
            y = element.height - (spark[index] * ratioH + margin);
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
