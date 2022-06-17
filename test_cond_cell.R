MAT = matrix(runif(100, -1, 1), nrow = 10,
             dimnames = list(LETTERS[1:10], LETTERS[1:10]))
diag(MAT) = 1
MAT[upper.tri(MAT)] = MAT[lower.tri(MAT)]
rhandsontable(MAT, readOnly = TRUE, width = 750, height = 300) %>%
  hot_cols(renderer = "
           function (instance, td, row, col, prop, value, cellProperties) {
             Handsontable.renderers.NumericRenderer.apply(this, arguments);
#             if (row == col) {
#              td.style.background = 'lightgrey';
#             } else if (col > row) {
#              td.style.background = 'grey';
#              td.style.color = 'grey';
#             } else if (value < -0.75) {
#              td.style.background = 'pink';
             } else if (value > 0.75) {
              td.style.background = 'lightgreen';
             }
           }")