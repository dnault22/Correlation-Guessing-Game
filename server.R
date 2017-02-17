library(shiny)

if (!"MASS" %in% installed.packages()) install.packages("MASS")
library(MASS) 

shinyServer(function(input,output){
  
nr_obs2 = 50
nr_obs = 100

mu1  <- 100
mu2  <- 100
sig1 <- 100
sig2 <- 100
  
rValueChoices = c(-.99, -.9, -.8, -.7, -.6, -.5, -.3, 0.0, .3, .5, .6, .7, .8, .9, .99)
correctRange = c(-1.0,-.98, -.93, -.87, -.83, -.77, -.74, -.66, -.64,-.56, -.55, -.45, -.35, -.25, -.1, .1, .25, .35, .45, .55, .56, .64, .66, .74, .77, .83, .87, .93, .98, 1.0)
closeRange = c(-1.0, -.95, -.96, -.84, -.87, -.73, -.78, -.62, -.69, -.51, -.6, -.4, -.4, -.2, -.2, .2, .2, .4, .4, .6, .51, .69, .62, .78, .73, .87, .84, .96, .95, 1.0)
  
minCorrect = 0
maxCorrect = 0
minClose = 0
maxClose = 0
rLocation = sample(seq(1,length(closeRange)),1)
  
correct = -1
  
generatedValue <- reactiveValues(
    rValue = sample(rValueChoices,1))

baseValue <- reactiveValues(
    rValue = sample(rValueChoices,1))

  
returnValue <- reactiveValues(
    messageToReturn = "")
  
checker <- reactiveValues(
    answerChecked = 0)

valueForPlot3 <- reactiveValues(
    numRight = {})
  
valueForPlot <- reactiveValues(
    numGuessed = 1)

valueCorrect <- reactiveValues(
    correct = 0)
  
guessPlot <- reactiveValues(
    guess = {})

guessPlot2 <- reactiveValues (
    guess2 = {})
  
guessPlot3 <- reactiveValues (
    guess3 = {})
  
valueForPlot2 <- reactiveValues(
    numGuessedRight = 0)
  
observeEvent(input$newPlot, {
    baseValue$rValue = sample(rValueChoices,1) 
})
  
observeEvent(input$resetScore, {
    valueForPlot3$numRight = {}
    valueForPlot$numGuessed = 1
    valueForPlot2$numGuessedRight = 0
    guessPlot$guess = {}
    guessPlot2$guess2= {}
    guessPlot3$guess3 = {}
})
  
observeEvent(input$newPlot, {
    sample(rValueChoices,1)
    rho  <- baseValue$rValue
    # Generate random shots
    shots <- mvrnorm(n=as.numeric(nr_obs),mu=c(mu1,mu2),Sigma=matrix(c(sig1,rho*100,rho*100,sig2),2))
    generatedValue$rValue <- cor(shots[,1],shots[,2])
    checker$answerChecked = 0
    returnValue$messageToReturn = ""
})
  
observeEvent(input$checkAnswer, {
  
#If they didn't already make a guess
    if(checker$answerChecked <= 0){
       guessPlot$guess[valueForPlot$numGuessed] = input$rho - generatedValue$rValue
       guessPlot2$guess2[valueForPlot$numGuessed] = input$rho
       guessPlot3$guess3[valueForPlot$numGuessed] = generatedValue$rValue
    
#Our original R value was -.99 or .99
    if(baseValue$rValue == -.99 | baseValue$rValue == .99)
      #abs(v1-v2)< alloted difference based on reference at top of code
      #rounding is needed for all values to ensure that the <= function works as intended
      #due to the nature of R, removing these rounding functions can result in false errors.
      	{if (round(abs(round(input$rho,2) - round(generatedValue$rValue,2))) <= .01)
	        	{returnValue$messageToReturn <- paste("That's correct! r = ", round(generatedValue$rValue,2))
        		  valueForPlot2$numGuessedRight = valueForPlot2$numGuessedRight + 1
   	      	  valueCorrect$correct <- 2
	            checker$answerChecked <- 1
	            valueForPlot3$numRight[valueForPlot$numGuessed]= valueForPlot2$numGuessedRight
		     }
         else if (round(abs(round(input$rho,2) - round(generatedValue$rValue,2))) <= .04)
	          	{returnValue$messageToReturn <- paste("You were close! r = ", round(generatedValue$rValue,2))
               checker$answerChecked <- 1
                valueCorrect$correct <- 1
                valueForPlot3$numRight[valueForPlot$numGuessed]= valueForPlot2$numGuessedRight
	      	}
       	else 
			          {returnValue$messageToReturn <- paste("That's incorrect. r = ", round(generatedValue$rValue,2))
      			     checker$answerChecked <- 1
      			     valueCorrect$correct <- 0
         		     valueForPlot3$numRight[valueForPlot$numGuessed]= valueForPlot2$numGuessedRight
	       	}
  	}

#Our original R value was -.9 or .9
    if(baseValue$rValue == -.9 | baseValue$rValue == .9)
	      {if (round(abs(round(input$rho,2) - round(generatedValue$rValue,2))) <= .03)
	        	{returnValue$messageToReturn <- paste("That's correct! r = ", round(generatedValue$rValue,2))
        		  valueForPlot2$numGuessedRight = valueForPlot2$numGuessedRight + 1
	            checker$answerChecked <- 1
	            valueCorrect$correct <- 2
	             valueForPlot3$numRight[valueForPlot$numGuessed]= valueForPlot2$numGuessedRight
	     	}
	      else if (round(abs(round(input$rho,2) - round(generatedValue$rValue,2))) <= .06)
		          {returnValue$messageToReturn <- paste("You were close! r = ", round(generatedValue$rValue,2))
          		 checker$answerChecked <- 1
          		 valueCorrect$correct <- 1
       			   valueForPlot3$numRight[valueForPlot$numGuessed]= valueForPlot2$numGuessedRight
	     	}
    	else
	        	{	returnValue$messageToReturn <- paste("That's incorrect. r = ", round(generatedValue$rValue,2))
           		checker$answerChecked <- 1
           		valueCorrect$correct <- 0
            	valueForPlot3$numRight[valueForPlot$numGuessed]= valueForPlot2$numGuessedRight
		  }

	}

#Our original R value was -.8 or .8
    if(baseValue$rValue == -.8 | baseValue$rValue == .8)
      	{if (round(abs(round(input$rho,2) - round(generatedValue$rValue,2))) <= .03)
		        {returnValue$messageToReturn <- paste("That's correct! r = ", round(generatedValue$rValue,2))
   	  	     valueForPlot2$numGuessedRight = valueForPlot2$numGuessedRight + 1
	           checker$answerChecked <- 1
	           valueCorrect$correct <- 2
	           valueForPlot3$numRight[valueForPlot$numGuessed]= valueForPlot2$numGuessedRight
	     	}
     	   else if (round(abs(round(input$rho,2) - round(generatedValue$rValue,2))) <= .07)
		          {returnValue$messageToReturn <- paste("You were close! r = ", round(generatedValue$rValue,2))
          		checker$answerChecked <- 1
          		valueCorrect$correct <- 1
       			  valueForPlot3$numRight[valueForPlot$numGuessed]= valueForPlot2$numGuessedRight
	  	  }
	      else
	          	{returnValue$messageToReturn <- paste("That's incorrect. r = ", round(generatedValue$rValue,2))
           		 checker$answerChecked <- 1
           		 valueCorrect$correct <- 0
            	 valueForPlot3$numRight[valueForPlot$numGuessed]= valueForPlot2$numGuessedRight
		     }

	}

#Our original R value was -.7 or .7
    if(baseValue$rValue == -.7 | baseValue$rValue == .7)
        	{if (round(abs(round(input$rho,2) - round(generatedValue$rValue,2))) <= .04)
	            	{returnValue$messageToReturn <- paste("That's correct! r = ", round(generatedValue$rValue,2))
   	      	    valueForPlot2$numGuessedRight = valueForPlot2$numGuessedRight + 1
	              checker$answerChecked <- 1
	              valueCorrect$correct <- 2
	              valueForPlot3$numRight[valueForPlot$numGuessed]= valueForPlot2$numGuessedRight
	      	}
	        else if (round(abs(round(input$rho,2) - round(generatedValue$rValue,2))) <= .08)
		            {returnValue$messageToReturn <- paste("You were close! r = ", round(generatedValue$rValue,2))
          			 checker$answerChecked <- 1
          			 valueCorrect$correct <- 1
       			     valueForPlot3$numRight[valueForPlot$numGuessed]= valueForPlot2$numGuessedRight
	      	}
	        else
	            	{returnValue$messageToReturn <- paste("That's incorrect. r = ", round(generatedValue$rValue,2))
           		   checker$answerChecked <- 1
           		   valueCorrect$correct <- 0
            		valueForPlot3$numRight[valueForPlot$numGuessed]= valueForPlot2$numGuessedRight
	      	}

  }
    
#Our original R value was -.6 or .6
    if(baseValue$rValue == -.6 | baseValue$rValue == .6)
	        {if (round(abs(round(input$rho,2) - round(generatedValue$rValue,2))) <= .04)
	            	{returnValue$messageToReturn <- paste("That's correct! r = ", round(generatedValue$rValue,2))
   	         	  valueForPlot2$numGuessedRight = valueForPlot2$numGuessedRight + 1
	              checker$answerChecked <- 1
	              valueCorrect$correct <- 2
	              valueForPlot3$numRight[valueForPlot$numGuessed]= valueForPlot2$numGuessedRight
	       	}
          	else if (round(abs(round(input$rho,2) - round(generatedValue$rValue,2))) <= .09)
	            	{returnValue$messageToReturn <- paste("You were close! r = ", round(generatedValue$rValue,2))
          			  checker$answerChecked <- 1
          			  valueCorrect$correct <- 1
       			      valueForPlot3$numRight[valueForPlot$numGuessed]= valueForPlot2$numGuessedRight
	       	}
	          else
	            	{returnValue$messageToReturn <- paste("That's incorrect. r = ", round(generatedValue$rValue,2))
                 checker$answerChecked <- 1
                  valueCorrect$correct <- 0
                  valueForPlot3$numRight[valueForPlot$numGuessed]= valueForPlot2$numGuessedRight
	      	}

	}

#Our base R value was -.5 or .5
    if(baseValue$rValue == -.5 | baseValue$rValue == .5)
        	{if (round(abs(round(input$rho,2) - round(generatedValue$rValue,2))) <= .05)
	            	{returnValue$messageToReturn <- paste("That's correct! r = ", round(generatedValue$rValue,2))
   		          valueForPlot2$numGuessedRight = valueForPlot2$numGuessedRight + 1
	              checker$answerChecked <- 1
	              valueCorrect$correct <- 2
	              valueForPlot3$numRight[valueForPlot$numGuessed]= valueForPlot2$numGuessedRight
	      	}
        	else if (round(abs(round(input$rho,2) - round(generatedValue$rValue,2))) <= .1)
	            	{returnValue$messageToReturn <- paste("You were close! r = ", round(generatedValue$rValue,2))
          			  checker$answerChecked <- 1
          			  valueCorrect$correct <- 1
       			     valueForPlot3$numRight[valueForPlot$numGuessed]= valueForPlot2$numGuessedRight
		      }
        	else
	            	{returnValue$messageToReturn <- paste("That's incorrect. r = ", round(generatedValue$rValue,2))
           		   checker$answerChecked <- 1
           		   valueCorrect$correct <- 0
            		valueForPlot3$numRight[valueForPlot$numGuessed]= valueForPlot2$numGuessedRight

	      	}

	}

#Our base R value was -.3 or .3
    if(baseValue$rValue == -.3 | baseValue$rValue == .3)
	        {if (round(abs(round(input$rho,2) - round(generatedValue$rValue,2))) <= .05)
	            	{returnValue$messageToReturn <- paste("That's correct! r = ", round(generatedValue$rValue,2))
   	        	  valueForPlot2$numGuessedRight = valueForPlot2$numGuessedRight + 1
	              checker$answerChecked <- 1
	              valueCorrect$correct <- 2
	              valueForPlot3$numRight[valueForPlot$numGuessed]= valueForPlot2$numGuessedRight
	      	}
	        else if (round(abs(round(input$rho,2) - round(generatedValue$rValue,2))) <= .1)
	            	{returnValue$messageToReturn <- paste("You were close! r = ", round(generatedValue$rValue,2))
          			 checker$answerChecked <- 1
          			 valueCorrect$correct <- 1
       			     valueForPlot3$numRight[valueForPlot$numGuessed]= valueForPlot2$numGuessedRight
	       	}
        	else
	            	{returnValue$messageToReturn <- paste("That's incorrect. r = ", round(generatedValue$rValue,2))
           		   checker$answerChecked <- 1
           		   valueCorrect$correct <- 0
            		 valueForPlot3$numRight[valueForPlot$numGuessed]= valueForPlot2$numGuessedRight
	      	}

	}

#Our original R value was 0
    if(baseValue$rValue == 0)
        	{if (round(abs(round(input$rho,2) - round(generatedValue$rValue,2))) <= .1)
	            	{returnValue$messageToReturn <- paste("That's correct! r = ", round(generatedValue$rValue,2))
   	        	  valueForPlot2$numGuessedRight = valueForPlot2$numGuessedRight + 1
	              checker$answerChecked <- 1
	              valueCorrect$correct <- 2
	              valueForPlot3$numRight[valueForPlot$numGuessed]= valueForPlot2$numGuessedRight
		      }
        	else if (round(abs(round(input$rho,2) - round(generatedValue$rValue,2))) <= .2)
             		 {returnValue$messageToReturn <- paste("You were close! r = ", round(generatedValue$rValue,2))
          			  checker$answerChecked <- 1
          			  valueCorrect$correct <- 1
       			       valueForPlot3$numRight[valueForPlot$numGuessed]= valueForPlot2$numGuessedRight
		      }
	       else
		            {returnValue$messageToReturn <- paste("That's incorrect. r = ", round(generatedValue$rValue,2))
           		   checker$answerChecked <- 1
           		   valueCorrect$correct <- 0
            		valueForPlot3$numRight[valueForPlot$numGuessed]= valueForPlot2$numGuessedRight
	      	}

	}

   
   # Now that we've covered all cases, if the difference was greater than 1
   # (which is outside of our graphs range, we want to give a special response) 
   # We only need to change the message as we know everything else was covered
   # Via our cases.
    if(abs(input$rho-round(generatedValue$rValue,2)) > 1)
        {returnValue$messageToReturn <- paste("That was a critical failure! Your guess was way off! r = ", round(generatedValue$rValue,2))
     }
    
  valueForPlot$numGuessed = valueForPlot$numGuessed + 1
  
  }
})
  
getResult <- function() {
     paste(returnValue$messageToReturn)
 }

output$guessResult <- renderText({
    getResult()
  })
  
output$correctness1 <- renderUI({
    if(valueCorrect$correct == -1)
         {HTML("")}
    else if(valueCorrect$correct == 0)
         {myOutput = paste('<font color="red">', getResult(),'</font>')
          HTML(myOutput)} #wrong
    else if(valueCorrect$correct == 1) #close
         {myOutput = paste('<font color="#C9960C">', getResult(), '</font>')
           HTML(myOutput)}
    else #correct
         {myOutput = paste('<font color="green">', getResult(), '</font>')
           HTML(myOutput)}
})
  
  
output$scatterplot <- renderPlot({

# Bivariate normal distribution parameters
rho  <- baseValue$rValue

# Generate random shots
shots <- mvrnorm(n=as.numeric(nr_obs),mu=c(mu1,mu2),Sigma=matrix(c(sig1,rho*100,rho*100,sig2),2))
generatedValue$rValue <- cor(shots[,1],shots[,2])
# Plot the shots
par(mar=c(4,4,1,1))
plot(shots, xlim=c(65,135),ylim=c(60,140), xlab="x",ylab="y",col="dark blue",pch=20, xaxs="i", yaxs="i", main=NULL)
})
  
# This function pre-fills the arrays with -3's because they are outside of the scope of
# the graph, but can still be plotted and won't result in NA spots regardless of
# current guesses.
output$results <- renderPlot({
    if(length(guessPlot$guess) == 0)
           {negThrees = {}
            for(i in 1:21){
                negThrees[i] = -3
             }
           par(mar=c(4,4,1,1))
           plot(x = seq(0,20), y = negThrees, ylim = c(-1,1), xlab = "Trial", ylab = "Difference (Guess - Actual)")
           abline(h=0)
        }
      
    else if(length(guessPlot$guess) == 1){
         negThrees = {}
         negThrees[1] = -3
         negThrees[2] = guessPlot$guess[1]
      
         for(i in 3:21){
            negThrees[i] = -3
           }
         par(mar=c(4,4,1,1)) 
         plot(x = seq(0,20), y = negThrees, ylim=c(-1,1), xlab = "Trial", ylab = "Difference (Guess - Actual)")
         abline(h=0)}
      
     else if(length(guessPlot$guess) <= 20){
          negThrees = {}
          negThrees[1] = -3
         for(i in 0:length(guessPlot$guess)+1){
             negThrees[i+1] = guessPlot$guess[i]
         }
         for(i in length(guessPlot$guess)+2:21){
             negThrees[i] = -3
         }
         par(mar=c(4,4,1,1))  
         plot(x = seq(0,20), y = negThrees[1:21], ylim=c(-1,1), xlab = "Trial", ylab = "Difference (Guess - Actual)")
         abline(h=0)
     }
  
    else{
         negThrees = {}
         negThrees[1] = -3
         for(i in 2:length(guessPlot$guess)+1){
             negThrees[i] = guessPlot$guess[i-1]
          }
          par(mar=c(4,4,1,1))
          plot(x = seq(0,length(guessPlot$guess)), y = negThrees, ylim=c(-1,1), xlab = "Trial", ylab = "Difference (Guess - Actual)")
         abline(h=0)}
  
})
  
output$secondResults <- renderPlot({
    if(length(guessPlot2$guess2) == 0)
        {par(mar=c(4,4,1,1))
        plot(-3, xlim=c(-1,1),ylim = c(-1,1), xlab = "Actual", ylab = "Guess")
         lines(x= seq(-2,2), y=seq(-2,2) ,type="l", col="blue")
    }
  
    else{
        par(mar=c(4,4,1,1))
        plot(y = guessPlot2$guess2 , x = guessPlot3$guess3, xlim = c(-1,1),ylim=c(-1,1), xlab = "Actual", ylab = "Guess")
       lines(x= seq(-2,2), y=seq(-2,2) ,type="l", col="blue")
     }
  })

})
