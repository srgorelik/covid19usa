#!/usr/bin/env Rscript

# Visualize US Covid-19 cases using NY Times data.
# R Shiny App by Seth Gorelik.

library(shiny)
library(shinythemes)
library(shinyWidgets)
library(dplyr)
library(plotly)

us.df <- read.csv(url('https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv'), stringsAsFactors = F)

ui <- fluidPage(

	tags$head(
		tags$meta(name = 'viewport', content = 'max-width=800'), # to force desktop view on mobile device
		tags$style(
			type = 'text/css',
			'.container-fluid {
				max-width: 95%;
			}
			a:link, a:visited {
				color: #0042ad;
				text-decoration: none;
			}
			a:hover, a:active {
				color: grey;
				text-decoration: none;
			}
			#sidebar {
				background-color: white;
				padding: 0px;
			}
			#spacer {
				font-size: 0;
				height: 50px;
				line-height: 0;
			}
			#footer {
				position: fixed;
				bottom: 0;
				right: 0;
				left: 0;
				background: #f0f0f0;
				padding: 10px;
				box-sizing: border-box;
				text-align: center;
				font-size: x-small;
			}'
		)
	),

	theme = shinytheme('cosmo'),

	fluidRow(
		column(12, align = 'center',

			div(id = 'spacer'),

			titlePanel('Covid-19 in the United States'),

			div(id = 'spacer'),

			plotlyOutput('plotlybarchart', width = '60%'),

			div(id = 'spacer'),

			pickerInput(
				inputId = 'states',
				label = NULL,
				width = '50%',
				choices = levels(as.factor(us.df$state)),
				selected = levels(as.factor(us.df$state)),
				multiple = T,
				options = pickerOptions(
					actionsBox = T,
					size = 6,
					selectedTextFormat = 'count > 2',
					liveSearch = T,
					liveSearchPlaceholder = 'Search state/territory...',
					countSelectedText = '{0} states/territories selected'
				),
			),

			div(id = 'spacer'),
			div(id = 'spacer')

		)
	),

	div(id = 'footer',
		HTML(
			paste0(
				'Data from ',
				a('The New York Times', href = 'https://github.com/nytimes/covid-19-data', target = '_blank'),
				' based on reports from state and local health agencies, and updated regularly.<br>',
				'Visualization by ',
				a('srgorelik', href = 'https://github.com/srgorelik/covid19usa', target = '_blank'),
				', powered by ',
				a('shinyapps.io', href = 'https://www.shinyapps.io/', target = '_blank'),
				'.'
			)
		)
	)
)

server <- function(input, output) {

	subset.data <- reactive({
		req(input$states)
		us.df %>%
			mutate(date = as.Date(date, format = '%Y-%m-%d')) %>%
			filter((state %in% input$states)) %>%
			as.data.frame()
	})

	output$plotlybarchart <- renderPlotly({
		tmp.df <- subset.data() %>%
			select(-state, -fips) %>%
			group_by(date) %>%
			summarize_all(sum)
		plot_ly(tmp.df, x = ~date, y = ~cases, type = 'bar', name = 'Cases', color = I('red')) %>%
			add_trace(tmp.df, x = ~date, y = ~deaths, type = 'bar', name = 'Deaths', color = I('black')) %>%
			layout(barmode = 'overlay',
				hovermode = 'x',
				autosize = T,
				margin = list(l = 50, r = 50, b = 0, t = 50, pad = 4),
				legend = list(orientation = 'h', xanchor = 0, x = 0, y = 100),
				xaxis = list(title = ''),
				yaxis = list(title = 'Cumulative Count\n&nbsp;', zerolinecolor = toRGB('grey92'), tickformat = ',d')) %>%
			config(showTips = F,
				   displaylogo = F,
				   modeBarButtonsToRemove = c('zoom2d', 'zoomIn2d', 'zoomOut2d', 'pan2d', 'lasso2d', 'autoScale2d', 'select2d'),
				   toImageButtonOptions = list(format = 'png', width = 800, height = 425, res = 300, units = 'px'))
	})

}

shinyApp(ui, server)