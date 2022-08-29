### Custom function to format betas, CIs and p-values
# Provide the data.frame (df), and character

beta.format <- function(df,beta,se="",alpha=.05,cilb="",ciub="",pval,exp=FALSE,
                        digits=3,pvaldigits=4,format="f",keeporig=TRUE,estci=TRUE){
  
  # Check that only one of SE or CILB/CIUB are provided
  if(se!="" & (cilb!="" | ciub!="")){
    stop("Specify only se or cilb/ciub arguments, not both")
  }

  # If SE is provided, then calculate CI bounds
  if(se!=""){
    critvalue <- qnorm(1-(alpha/2))
    df <- df %>%
      mutate(cilbvar = get(beta)-(critvalue*get(se)),
             ciubvar = get(beta)+(critvalue*get(se)))
    cilb <- "cilbvar"
    ciub <- "ciubvar"
  }
  # Create temp intermediate variables for formatting
  if(exp==TRUE){
    df <- df %>%
      mutate(beta.temp=exp(get(beta)),
             cilb.temp=exp(get(cilb)),
             ciub.temp=exp(get(ciub)))
  }
  if(exp==FALSE){
    df <- df %>%
      mutate(beta.temp=get(beta),
             cilb.temp=get(cilb),
             ciub.temp=get(ciub))
  }
  # Create formatted versions
  if(estci==TRUE){
    df <- df %>%
      mutate(est.fmt = paste0(formatC(beta.temp,digits=digits,format="f"), " (",
                              formatC(cilb.temp,digits=digits,format="f"), " - ",
                              formatC(ciub.temp,digits=digits,format="f"), ")"),
             pval.fmt = formatC(get(pval),digits=pvaldigits,format="f")
      )
  }
  if(estci==FALSE){
    df <- df %>%
      mutate(est.fmt = paste0(formatC(beta.temp,digits=digits,format="f")),
             ci.fmt = paste0(formatC(cilb.temp,digits=digits,format="f"), " - ",
                             formatC(ciub.temp,digits=digits,format="f")),
             pval.fmt = formatC(get(pval),digits=pvaldigits,format="f")
      )
  }
  
  
  # Drop intermediate variables
  df <- df %>% select(-any_of(c("beta.temp","cilb.temp","ciub.temp")))
  if(se!=""){
    df <- df %>% select(-any_of(c("cilbvar","ciubvar")))
  }
  if(keeporig==FALSE){
    df <- df %>% select(-any_of(c(beta,se,pval,cilb,ciub)))
  }
  
  return(df)
}


# Test run
if(FALSE){
  testdf <- data.frame(estimate=rgamma(n=6,2),SE=rbeta(n=6,3,5),p.value=rbeta(n=6,1,2)) %>%
    mutate(LCI=estimate-qnorm(0.975),UCI=estimate+qnorm(0.975))
  beta.format(df=testdf,
              beta="estimate",se="SE",pval="p.value",
              exp=FALSE,keeporig=TRUE,estci=TRUE)

  beta.format(df=testdf,
              beta="estimate",cilb="LCI",ciub="UCI",pval="p.value",
              exp=FALSE,keeporig=TRUE,estci=TRUE)
  
}

### Figure this out
# runGitHub(repo="tools",username="antonpalma",subdir="beta.format")

