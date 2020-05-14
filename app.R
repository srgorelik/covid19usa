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
		tags$meta(name = 'viewport', content = 'max-width=1000'), # to force desktop view on mobile device
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
			   titlePanel('United States COVID-19 Cases'),
			   div(id = 'spacer')
		)
	),

	sidebarLayout(

		sidebarPanel(

			width = 3,

			p('Time series:'),

			radioButtons(
				inputId = 'timeSeriesType',
				label = NULL,
				choices = c('Total Count', 'Daily Count', '7-day Average', 'Death Rate'),
				selected = 'Total Count',
				inline = F
			),

			br(),

			p('States/territories:'),

			pickerInput(
				inputId = 'states',
				label = NULL,
				choices = levels(as.factor(us.df$state)),
				selected = levels(as.factor(us.df$state)),
				multiple = T,
				options = pickerOptions(
					actionsBox = T,
					dropupAuto = F,
					size = 6,
					selectedTextFormat = 'count > 2',
					liveSearch = T,
					liveSearchPlaceholder = 'Search state/territory...',
					countSelectedText = '{0} selected'
				),
			)

		),

		mainPanel(
			width = 9,
			plotlyOutput('plot')
		)
	),

	div(id = 'spacer'),
	div(id = 'spacer'),

	div(id = 'footer',
		HTML(
			paste0(
				'Data from ',
				a('The New York Times', href = 'https://github.com/nytimes/covid-19-data', target = '_blank'),
				' based on reports from state and local health agencies, and updated regularly. Visualization by ',
				a('srgorelik', href = 'https://github.com/srgorelik/covid19usa', target = '_blank'),
				', powered by ',
				a('shinyapps.io', href = 'https://www.shinyapps.io/', target = '_blank'),
				'.<br>If you can, please donate to ',
				a('Direct Relief', href = 'https://www.directrelief.org/', target = '_blank'),
				' or your ',
				a('local food bank', href = 'https://www.feedingamerica.org/', target = '_blank'),
				'.'
			)
		)
	)
)

server <- function(input, output) {

	subset.data <- reactive({
		req(input$states)
		us.df %>%
			rename(cum_cases = cases, cum_deaths = deaths) %>%
			mutate(date = as.Date(date, format = '%Y-%m-%d')) %>%
			filter((state %in% input$states)) %>%
			as.data.frame()
	})

	output$plot <- renderPlotly({

		tmp.df <- subset.data() %>%
			select(-state, -fips) %>%
			group_by(date) %>%
			summarize_all(sum) %>%
			arrange(date) %>%
			mutate(
				daily_cases = c(cum_cases[1], (cum_cases - lag(cum_cases))[-1]),
				daily_deaths = c(cum_deaths[1], (cum_deaths - lag(cum_deaths))[-1]),
				daily_cases_7day_mean = TTR::runMean(daily_cases, 7),
				daily_deaths_7day_mean = TTR::runMean(daily_deaths, 7),
				daily_cases_7day_mean = ifelse(is.na(daily_cases_7day_mean), 0, daily_cases_7day_mean),
				daily_deaths_7day_mean = ifelse(is.na(daily_deaths_7day_mean), 0, daily_deaths_7day_mean),
				cum_d2c_rate = round(cum_deaths/cum_cases*100, 2),
				cum_s2c_rate = round((cum_cases-cum_deaths)/cum_cases*100, 2) # is this correct??
			) %>%
			as.data.frame()

		# not sure if "survival rate" is accurate
		surv.rate <- plot_ly(tmp.df, x = ~date, y = ~cum_s2c_rate, type = 'scatter', mode = 'lines', name = 'Deaths', color = I('red3'), hovertext = paste0(tmp.df$cum_s2c_rate, '%'), hoverinfo = 'text+x', showlegend = F) %>%
			layout(hovermode = 'x',
				   autosize = T,
				   margin = list(l = 0, r = 50, b = 0, t = 0, pad = 4),
				   legend = list(orientation = 'h', xanchor = 0, x = 0, y = 100, traceorder = 'reversed'),
				   xaxis = list(title = '', range = c(as.Date('2020-02-28', format = '%Y-%m-%d'), max(tmp.df$date))),
				   yaxis = list(title = 'Survival Rate\n&nbsp;', zerolinecolor = toRGB('grey92'), tickformat = '.1f', ticksuffix = '%', showticksuffix = 'all')) %>%
			config(showTips = F,
				   displaylogo = F,
				   modeBarButtonsToRemove = c('hoverCompareCartesian', 'hoverClosestCartesian', 'toImage', 'zoom2d', 'zoomIn2d', 'zoomOut2d', 'pan2d', 'lasso2d', 'autoScale2d', 'select2d'))

		death.rate <- plot_ly(tmp.df, x = ~date, y = ~cum_d2c_rate, type = 'scatter', mode = 'lines', name = 'Deaths', color = I('grey30'), hovertext = paste0(tmp.df$cum_d2c_rate, '%'), hoverinfo = 'text+x', showlegend = F) %>%
			layout(hovermode = 'x',
				   autosize = T,
				   margin = list(l = 0, r = 50, b = 0, t = 0, pad = 4),
				   legend = list(orientation = 'h', xanchor = 0, x = 0, y = 100, traceorder = 'reversed'),
				   xaxis = list(title = '', range = c(as.Date('2020-02-28', format = '%Y-%m-%d'), max(tmp.df$date))),
				   yaxis = list(title = 'Death Rate\n&nbsp;', zerolinecolor = toRGB('grey92'), tickformat = '.1f', ticksuffix = '%', showticksuffix = 'all')) %>%
			config(showTips = F,
				   displaylogo = F,
				   modeBarButtonsToRemove = c('hoverCompareCartesian', 'hoverClosestCartesian', 'toImage', 'zoom2d', 'zoomIn2d', 'zoomOut2d', 'pan2d', 'lasso2d', 'autoScale2d', 'select2d'))

		daily.lines <- plot_ly(tmp.df, x = ~date, y = ~daily_deaths_7day_mean, type = 'scatter', mode = 'lines', name = 'Deaths', color = I('black'), hovertext = scales::comma(tmp.df$daily_deaths_7day_mean), hoverinfo = 'text+x') %>%
			add_trace(tmp.df, x = ~date, y = ~daily_cases_7day_mean, type = 'scatter', mode = 'lines', name = 'Cases', color = I('red'), hovertext = scales::comma(tmp.df$daily_cases_7day_mean), hoverinfo = 'text+x') %>%
			layout(hovermode = 'x',
				   autosize = T,
				   margin = list(l = 0, r = 50, b = 0, t = 0, pad = 4),
				   legend = list(orientation = 'h', xanchor = 0, x = 0, y = 100, traceorder = 'reversed'),
				   xaxis = list(title = ''),
				   yaxis = list(title = '7-day Average Count\n&nbsp;', zerolinecolor = toRGB('grey92'), tickformat = ',d')) %>%
			config(showTips = F,
				   displaylogo = F,
				   modeBarButtonsToRemove = c('hoverCompareCartesian', 'hoverClosestCartesian', 'toImage', 'zoom2d', 'zoomIn2d', 'zoomOut2d', 'pan2d', 'lasso2d', 'autoScale2d', 'select2d'))

		daily.bars <- plot_ly(tmp.df, x = ~date, y = ~daily_cases, type = 'bar', name = 'Cases', color = I('red'), hovertext = scales::comma(tmp.df$daily_cases), hoverinfo = 'text+x') %>%
			add_trace(tmp.df, x = ~date, y = ~daily_deaths, type = 'bar', name = 'Deaths', color = I('black'), hovertext = scales::comma(tmp.df$daily_deaths), hoverinfo = 'text+x') %>%
			layout(barmode = 'overlay',
				   hovermode = 'x',
				   autosize = T,
				   margin = list(l = 0, r = 50, b = 0, t = 0, pad = 4),
				   legend = list(orientation = 'h', xanchor = 0, x = 0, y = 100),
				   xaxis = list(title = ''),
				   yaxis = list(title = 'Count\n&nbsp;', zerolinecolor = toRGB('grey92'), tickformat = ',d')) %>%
			config(showTips = F,
				   displaylogo = F,
				   modeBarButtonsToRemove = c('hoverCompareCartesian', 'hoverClosestCartesian', 'toImage', 'zoom2d', 'zoomIn2d', 'zoomOut2d', 'pan2d', 'lasso2d', 'autoScale2d', 'select2d'))

		cum.bars <- plot_ly(tmp.df, x = ~date, y = ~cum_cases, type = 'bar', name = 'Cases', color = I('red'), hovertext = scales::comma(tmp.df$cum_cases), hoverinfo = 'text+x') %>%
			add_trace(tmp.df, x = ~date, y = ~cum_deaths, type = 'bar', name = 'Deaths', color = I('black'), hovertext = scales::comma(tmp.df$cum_deaths), hoverinfo = 'text+x') %>%
			layout(barmode = 'overlay',
				   hovermode = 'x',
				   autosize = T,
				   margin = list(l = 0, r = 50, b = 0, t = 0, pad = 4),
				   legend = list(orientation = 'h', xanchor = 0, x = 0, y = 100),
				   xaxis = list(title = ''),
				   yaxis = list(title = 'Cumulative Count\n&nbsp;', zerolinecolor = toRGB('grey92'), tickformat = ',d')) %>%
			config(showTips = F,
				   displaylogo = F,
				   modeBarButtonsToRemove = c('hoverCompareCartesian', 'hoverClosestCartesian', 'toImage', 'zoom2d', 'zoomIn2d', 'zoomOut2d', 'pan2d', 'lasso2d', 'autoScale2d', 'select2d'))

		if (input$timeSeriesType == 'Total Count') {
			cum.bars
		} else if (input$timeSeriesType == 'Daily Count') {
			daily.bars
		} else if (input$timeSeriesType == '7-day Average') {
			daily.lines
		} else if (input$timeSeriesType == 'Death Rate') {
			death.rate
		}

	})

}

shinyApp(ui, server)