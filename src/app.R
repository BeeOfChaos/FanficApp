library(shiny)
library(DT)
library(htmltools)

source("ao3_csv.R")



ui <- navbarPage(
	h5("All my favorite Fics!", style = "color: #990000; font-weight: bold; font-size: 100%; "),
	id = "navbar",

	tabPanel(
		p(span("*", style = "color: #0000ff;"), span("*", style = "color: #00ffff;"), span("*", style = "color: #33cc33;"), span("*", style = "color: #ffff00;"), span("*", style = "color: #ff9933;"), span("*", style = "color: #ff0000;"), span("*", style = "color: #ff99ff;"), span("*", style = "color: #cc00ff;"), style =  "font-weight: bold; font-size: 200%;"),

		fluidPage(
			tags$head(
				tags$style(".pagination {float: right;}"						
				)
			),

			#	part of custom pagination
			fluidRow(
				div(id = "pagination", 
					div(style = "display:inline-block;", 
						tags$a(id = "first", style = "cursor: pointer; color: #990000; font-weight:bold;", "First")),
					div(style = "display:inline-block;", 
						tags$a(id = "previous", style = "cursor: pointer; color: #00b339; font-weight:bold;", " Previous")),
					div(style = "display:inline-block;", 
						tags$input(id = "page", type = "number", class = "input-sm", value = "1", min = "1")),
					div(style = "display:inline-block;", 
						tags$span(id = "of")),
					div(style = "display:inline-block;", 
						tags$a(id = "next", style = "cursor: pointer; color: #00b339; font-weight:bold;", "Next ")),
					div(style = "display:inline-block;", 
						tags$a(id = "last", style = "cursor: pointer; color: #990000; font-weight:bold;", "Last"))
				)
			),		

			fluidRow(
				column(12,
						DT::dataTableOutput("completeTable"),
						br(),
						br(),
						br()
				)
			)
		)
	),

	tabPanel(title = "Quit", value = "stop", icon = icon("circle-notch"))
)

server <- shinyServer(function(input, output, session) {
	
	output$completeTable <- DT::renderDataTable ({
		header.style 	<- "th { font-family: 'Arial'; font-weight: bold; color: white; background-color: #990000;}"
		header.names 	<- c(" ", " ", colnames(complete_Table))							###	rowName, nest, colNames		
		odd.cell.style	<- "table tr:nth-child(odd)  td {background-color: #F7FFFA; }"				###	red:		"table tr:nth-child(odd)  td {background-color: #FFFCFC; }"
		even.cell.style	<- "table tr:nth-child(even) td {background-color: #EBFFF2; }"				###	red:		"table tr:nth-child(even) td {background-color: #FFF5F5; }"
		search.style	<-	"input.dtsp-search {color: #990000 !important;	}
							 input.dtsp-search::placeholder { color: #003312 !important; font-weight: bold; text-transform:capitalize; text-decoration-line: underline; }"
		my.container 	<- withTags(table(
			style(type = "text/css", header.style, even.cell.style, odd.cell.style, search.style),
			thead(
				tr(
					lapply(header.names, th, style = "text-align: center; border-right-width: 1px; border-right-style: solid; border-right-color: white; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: white")
				)
			)
		))

		datatable( 
			cbind(' ' = '&oplus;', complete_Table), escape = -2,							### nest, otherwise only complete_Table		
			filter = 'top', 
			options = list(
				dom = "PQlfrti<'pagination'>",
				pageLength = 10, 

				searchPanes = list(
					threshold = 1,
					columns = list(tag_col, fand_col, pers_col, word_col)
					#, cascadePanes = TRUE				####	slows program down when options in Searchpanes are selected!!!
					#	, viewTotal = TRUE				####	slows program down when options in Searchpanes are selected!!!				
				),

				search = list(
					return = TRUE
				),

				columnDefs = list( 	
					list(width = '30px', targets = c(1,3,9,11,12)), 		
					list(width = '180px', targets = c(5,6)),
					list(width = '100px', targets = c(2,4,8,10)),

					list(visible = FALSE, targets = c(13)),  								###	nest   summary
                	list(orderable = FALSE, className = 'details-control', targets = 1),	### nest

					list(
                        targets = tag_col,           
                        render = JS(
                            "function (data, type, row) {",
                            "  if (type === 'sp') {",
                            "    return data.split(', ');",
                            "  }",
                            "  return data;",
                            "}"
                        ),
                        searchPanes = list(orthogonal = "sp")
                    ),
					
					list(
                        targets = fand_col,           
                        render = JS(
                            "function (data, type, row) {",
                            "  if (type === 'sp') {",
                            "    return data.split(', ');",
                            "  }",
                            "  return data;",
                            "}"
                        ),
                        searchPanes = list(orthogonal = "sp")
                    ),

					list(
                        targets = pers_col,       
                        render = JS(
                            "function (data, type, row) {",
                            "  if (type === 'sp') {",
                            "    return data.split(', ');",
                            "  }",
                            "  return data;",
                            "}"
                        ),
                        searchPanes = list(orthogonal = "sp")
                    ),

					list(
						searchPanes = list (
							show = TRUE,
							#combiner = 'and',		###		AND LOGIC
							options = list(
								list(
									label = "Take over my life, will you? - over 100000",
									value = JS("function(rowData, rowIdx) { return (rowData[9] > 100000); }"
									)
								),
								list(
									label = "Epic - over 50000",
									value = JS("function(rowData, rowIdx) { return (rowData[9] > 50000); }"
									)
								),
								list(
									label = "Comfortable - 10000-50000",
									value = JS("function(rowData, rowIdx) { return (10000 <= rowData[9] && rowData[9] <= 50000); }"
									)
								),
								list(
									label = "A teeny-weeny fic, please - below 10000",
									value = JS("function(rowData, rowIdx) { return (rowData[9] < 10000); }"
									)
								)
							)
						),
						targets = word_col	
					)
				),
				#	part of custom pagination
				initComplete = JS("function(settings, json){  var table = settings.oInstance.api();  var pageinfo = table.page.info();  $('#of').text('of ' + pageinfo.pages);}")
			),
			callback = JS(
				#	part of nesting rows
                "table.column(1).nodes().to$().css({cursor: 'pointer'});
                var format = function(d) {
                    return '<div style=\"border: 2px solid red; border-radius: 8px; border-style: solid; border-color: #990000; padding: .5em;\"> Summary: ' +
                            d[13] + '</div>';
                };
                table.on('click', 'td.details-control', function() {
                    var td = $(this), row = table.row(td.closest('tr'));
                    if (row.child.isShown()) {
                    	row.child.hide();
                    	td.html('&oplus;');
                    } else {
                    	row.child(format(row.data())).show();
                    	td.html('&CircleMinus;');
                    }
                });",
				#	part of custom pagination
				"$('div.pagination').append($('#pagination')); $('#first').on('click', function(){ table.page('first').draw('page'); $('#page').val(1); });",
				"$('#previous').on('click', function(){ table.page('previous').draw('page'); $('#page').val(table.page.info().page + 1); });",
				"$('#next').on('click', function(){ table.page('next').draw('page'); $('#page').val(table.page.info().page + 1); });",
				"$('#last').on('click', function(){ table.page('last').draw('page'); $('#page').val(table.page.info().pages);});",
				"$('#page').on('change', function(){ var page = parseInt($('#page').val());",
				"  if(!isNaN(page)){ table.page(page-1).draw('page'); } });"
			),
			container = my.container,						
			extensions = c('Select', 'SearchPanes', 'SearchBuilder'),
			selection = 'none'
		)
	}, server = FALSE)				#	needed for searchpanes



	observe({
		if (input$navbar == "stop") 
			stopApp()
	})

})


shinyApp(ui = ui, server = server)
