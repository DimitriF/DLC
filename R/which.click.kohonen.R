##' Function to know which cell was click, used in the shiny apps
##'
##' @param grid.x horizontal size of the map
##' @param grid.y vertical size of the map
##' @param topo topo of the map
##' @param click.x x coordonate of the click
##' @param click.y y coordonate of the click
##' @author Dimitri Fichou
##' @export
##'

## Use in the ui to know which unit was clicked
which.click.kohonen <- function(grid.x,grid.y,topo,click.x,click.y){
  which.min(apply(somgrid(grid.x,grid.y,topo)$pts,1,function(x){sum((x[1]-click.x)^2,(x[2]-click.y)^2)}))
}
