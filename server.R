library(shiny)
library(DT)
library(ggplot2)
library(shinydashboard)
library(plotly)
shinyServer(function(input, output) {
    output$table <- DT::renderDataTable({
        selectRows(
            dataTableProxy("table"), 
            selected=1
        )
        DT::datatable(
            data[input$columns],filter = "top",selection = "single"
        )
    })
    output$reviews <- renderPlotly({
        if(length(input$table_rows_selected)>0){
            data<-data.frame(
                "category"=c(
                    "positive",
                    "negative"
                ),
                "amount"=c(
                    data[input$table_rows_selected,]$positive_ratings,
                    data[input$table_rows_selected,]$negative_ratings
                )
            )
            plot_ly(
                data, 
                labels = ~category, 
                values = ~amount, 
                type = 'pie',
                marker = list(colors = c('green', 'red'))
            )
        }
    })
    output$game_info <- renderUI({
        if(length(input$table_rows_selected)>0){
            name<-data_desc[input$table_rows_selected,]$name
            date<-paste("Release date: ",data_desc[input$table_rows_selected,]$release_date)
            dev<-paste("Developer: ",data_desc[input$table_rows_selected,]$developer)
            owners<-paste("Estimated number of owners: ",data_desc[input$table_rows_selected,]$owners)
            price<-paste("Price: ",data_desc[input$table_rows_selected,]$price," $")
            description<-data_desc[input$table_rows_selected,]$short_description
            HTML(paste(name,description,date,dev,owners,price,sep="<br>"))
        }
        else
        {
            HTML("Select game from the table above to see more info.")
        }
    })
    output$Dvalue <- renderPlotly({
        if(input$filter){
            dataD<-dataD%>%
                filter(min_owners>10000)
        }
        dataD<-dataD%>%
            filter(genres == input$genre)%>%
            filter(platforms == input$platform)%>%
            arrange(DollarsPerHour)%>%
            head(10)
        ggplotly(
            ggplot(
                dataD,
                aes(
                    x=reorder(name,DollarsPerHour),
                    y=DollarsPerHour,
                    fill=DollarsPerHour,
                    text=paste("Game: ",name,"<br>Price: ",price,"$<br>Playtime: ",average_playtime,"h<br>Dollars per hour: ",DollarsPerHour,"$/h"))
                ) + geom_bar(stat="identity")
            + ggtitle("Dollars per hour of playtime")
            +theme(axis.text.x = element_text(angle = 45, hjust = 1,vjust=1))
            +scale_fill_gradient(low = "green", high = "yellow")
            +scale_y_continuous(labels = function(x) sprintf("%.4f$", x))
            +xlab("Game")
            +ylab("Dollars per hour"),tooltip = "text")
        })
    output$Pvalue <- renderPlotly({
        if(input$filter){
            dataP<-dataP%>%
                filter(min_owners>10000)
        }
        dataP<-dataP%>%
            filter(genres == input$genre)%>%
            filter(platforms == input$platform)%>%
            arrange(NegativeRatio)%>%
            head(10)
        ggplotly(
            ggplot(
                dataP,
                aes(
                    x=reorder(name,NegativeRatio),
                    y=NegativeRatio,
                    fill=NegativeRatio,
                    text=paste("Game: ",name,"<br>Positive ratings: ",positive_ratings,"<br>Negative ratings: ",negative_ratings,"<br>Positive ratio: ",PositiveRatio,"<br>Negative ratio: ",NegativeRatio))
                ) + geom_bar(stat="identity") 
            + ggtitle("Share of negative reviews")
            + theme(axis.text.x = element_text(angle = 45, hjust = 1,vjust=1))
            + scale_fill_gradient(low = "green", high = "yellow")
            + scale_y_continuous(labels = function(x) sprintf("%.0f%%", 100*x))
            + xlab("Game")
            +ylab("Share of negative reviews"),tooltip = "text")
    })
    global <- reactiveValues(toHighlight = c(TRUE,rep(FALSE, length(dataH$year)-1)), 
                             selectedBar = "1998")
    observeEvent(eventExpr = input$history_click, {
        global$selectedBar <- dataH$year[round(input$history_click$x)]
        global$toHighlight <- dataH$year %in% global$selectedBar
    })
    output$history <- renderPlot({
        ggplot(
            dataH,
            aes(
                x=year,
                y=PositiveRatio, 
                fill = ifelse(global$toHighlight, yes = "yes", no = "no")
                )
            ) + geom_bar(stat="identity") + 
            ggtitle("Average ratio of positive reviews over the years") + 
            theme(axis.text.x = element_text(angle = 45, hjust = 1,vjust=1))+ 
            scale_fill_manual(values = c("yes" = "green", "no" = "grey"),guide="none")+
            ylab("Share of positive reviews")+
            scale_y_continuous(labels = function(x) sprintf("%.0f%%", 100*x))
    })
    output$history_table <- DT::renderDataTable({
        dataTMP<-data%>%
            filter(year==global$selectedBar)%>%
            arrange(-PositiveRatio)
        DT::datatable(
            dataTMP[c("name","developer","publisher","PositiveRatio")],
            filter = "top",selection = "single",
            options = list(pageLength = 5)
        )
    })
    output$prices <- renderPlotly({
        dataTMP<-data%>%
            filter(price>0)%>%
            group_by(year)%>%
            summarise(price=mean(price))
        plt <- ggplot(
            dataTMP,
            aes(
                x=year,
                y=price,
                group=1,
                text=paste("Year: ",year,"<br>Mean price: ",price,"$")
                )
            ) + ggtitle("Prices of games over the years")+ 
            theme(axis.text.x = element_text(angle = 45, hjust = 1,vjust=1))+
            ylab("Mean price")+
            xlab("Year")+
            scale_y_continuous(labels = function(x) sprintf("%.0f$", x))
        if("add area" %in% input$custom){
            plt<- plt + geom_area(fill)
        }
        if("add line" %in% input$custom){
            plt <- plt + geom_line(color="black")
        }
        plt <- plt + geom_point(color="black")
        ggplotly(plt,tooltip = "text")
    })
    output$hall_of_fame1 <- renderPlotly({
        dataTMP<-data%>%
            separate_rows(developer,sep=";")%>%
            separate_rows(genres,sep=";")%>%
            filter(release_date>input$time[1])%>%
            filter(release_date<input$time[2])%>%
            filter(genres == input$genrePD)%>%
            filter(min_owners > 10000)%>%
            group_by(developer)%>%
            summarise(
                no_games=n(), 
                NegativeRatio=sum(negative_ratings)/(sum(positive_ratings)+sum(negative_ratings))
            )%>%
            arrange(NegativeRatio)%>%
            head(15)
        ggplotly(
            ggplot(
                dataTMP,
                aes(
                    x=reorder(developer,NegativeRatio),
                    y=NegativeRatio,
                    fill=NegativeRatio,
                    text=paste("Developer: ",developer,"<br>Share of negative reviews: ",sprintf("%.2f%%", 100*NegativeRatio))
                )) + geom_bar(stat="identity") 
            + ggtitle(paste(input$genrePD,"games","developers with the lowest share of negative reviews between",input$time[1],"and",input$time[2]))
            + theme(axis.text.x = element_text(angle = 45, hjust = 1,vjust=1))
            + xlab("Developer")
            + scale_y_continuous(labels = function(x) sprintf("%.0f%%", 100*x))
            + scale_fill_gradient(low = "green", high = "yellow")
            + ylab("Share of negative reviews"), tooltip = "text")
    })
    output$hall_of_fame2 <- renderPlotly({
        dataTMP<-data%>%
            separate_rows(publisher,sep = ";")%>%
            separate_rows(genres,sep=";")%>%
            filter(release_date>input$time[1])%>%
            filter(release_date<input$time[2])%>%
            filter(genres == input$genrePD)%>%
            filter(min_owners > 10000)%>%
            group_by(publisher)%>%
            summarise(
                no_games=n(),
                NegativeRatio=sum(negative_ratings)/(sum(positive_ratings)+sum(negative_ratings))
            )%>%
            arrange(NegativeRatio)%>%
            head(15)
        ggplotly(
            ggplot(
                dataTMP,
                aes(
                    x=reorder(publisher,NegativeRatio),y=NegativeRatio,fill = NegativeRatio,text=paste("Publisher: ",publisher,"<br>Share of negative reviews: ",sprintf("%.2f%%", 100*NegativeRatio))
                    )
            ) + geom_bar(stat="identity") 
            + ggtitle(paste(input$genrePD ,"games","publishers with the lowest share of negative reviews between",input$time[1],"and",input$time[2]))
            + theme(axis.text.x = element_text(angle = 45, hjust = 1,vjust=1))
            +xlab("Publisher")
            +scale_y_continuous(labels = function(x) sprintf("%.0f%%", 100*x))
            +scale_fill_gradient(low = "green", high = "yellow")
            +ylab("Share of negative reviews"),tooltip = "text")
    })
    output$hall_of_fame3 <- renderPlotly({
        dataTMP<-data%>%
            separate_rows(developer,sep=";")%>%
            separate_rows(genres,sep=";")%>%
            filter(release_date>input$time[1])%>%
            filter(release_date<input$time[2])%>%
            filter(genres == input$genrePD)%>%
            filter(min_owners > 10000)%>%
            group_by(developer)%>%
            summarise(
                no_games=n(),
                PositiveRatio=sum(positive_ratings)/(sum(positive_ratings)+sum(negative_ratings)),
                no_players=sum(strtoi(min_owners)),
            )
        plot_ly(colors= c("red","green"),
            dataTMP,
            x=~no_players,
            y=~PositiveRatio,
            text=~developer,
            size=~no_games,
            color=~PositiveRatio,
            customdata=~no_games,
            mode="markers",
            marker=list(
                opacity=0.5,
                sizemode="area",
                sizeref=0.1,
                sizemin=3
            ),hovertemplate = paste(
                "<b>%{text}</b><br>",
                "Number of players: %{x}<br>",
                "Number of Games: %{customdata}<br>",
                "Ratio of positive reviews: %{marker.color:.4f}<br>"
            )
        )
    })
    output$hall_of_fame4 <- renderPlotly({
        dataTMP<-data%>%
            separate_rows(publisher,sep=";")%>%
            separate_rows(genres,sep=";")%>%
            filter(release_date>input$time[1])%>%
            filter(release_date<input$time[2])%>%
            filter(genres == input$genrePD)%>%
            filter(min_owners > 10000)
        dataTMP<-dataTMP%>%
            group_by(publisher)%>%
            summarise(
                no_games=n(),
                PositiveRatio=sum(positive_ratings)/(sum(positive_ratings)+sum(negative_ratings)),
                no_players=sum(strtoi(min_owners)),
            )
        plot_ly(colors= c("red","green"),
            dataTMP,
            x=~no_players,
            y=~PositiveRatio,
            text=~publisher,
            size=~no_games,
            color=~PositiveRatio,
            customdata=~no_games,
            mode="markers",
            marker=list(
                opacity=0.5,
                sizemode="area",
                sizeref=0.1,
                sizemin=3
            ),hovertemplate = paste(
                "<b>%{text}</b><br>",
                "Number of players: %{x}<br>",
                "Number of Games: %{customdata}<br>",
                "Ratio of positive reviews: %{marker.color:.4f}<br>"
            )
        )
    })
})
