
publish <- function()
{
  rsconnect::setAccountInfo(name='msudmannday',
                            token='***',
                            secret='***')
  rsconnect::deployApp('C:\\OneDrive\\BGSE\\DV\\distribution-of-uk-rental-rates')
}