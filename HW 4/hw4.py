# -*- coding: utf-8 -*-
"""
DATA 608 - Homework 4
Georgia Galanopoulos
Due: 3/25/2018
"""

"""
Background:
Enterococcus = fecal indicating bacteria. Lives in the intestines of humans
and other warm-blooded animals. The counts are useful as a water
quality indicator (low abundance in sewage-free environments). 

EPA reports counts as colonies (or cells) per 100ml of water.
Standard for unacceptable water quality is:
1) A single sample value > 110 Enterococcus/100 mL or
2) Five or more samples with a weighted average > 30 Enterococcus/100 mL.
"""

import dash
from dash.dependencies import Input, Output
import dash_html_components as html
import dash_core_components as dcc
from datetime import datetime as dt
import plotly.graph_objs as plgo
import numpy as np
from scipy.stats.mstats import gmean
import pandas as pd

url = "https://raw.githubusercontent.com/charleyferrari/CUNY_DATA_608/master/module4/Data/riverkeeper_data_2013.csv"
river = pd.read_csv(url)

river['Site'] = river.Site.astype('category')
river['Date'] = pd.to_datetime(river.Date)
river['EnteroCount'] = river.EnteroCount.str.lstrip('<>').astype('int')
#river = river.sort_values(by = 'Date')

river['P/FSingle'] = np.where((river.EnteroCount <= 110), 'Pass', 'Fail')
river['P/FGroup'] = np.where((river.groupby(['Date','Site']).EnteroCount.transform(lambda group: gmean(group.nlargest(5))) <= 30), 'Pass', 'Fail')
topriver = river.groupby(river['Site']).mean().sort('EnteroCount', ascending=True).head(10)
############################################################

hw4 = dash.Dash()

hw4.layout = html.Div(children = [
    html.H1('DATA 608 - Module 4'),
    html.H2('Question 1: Site Reccomendation for Kayakers'),
    html.Div([
        dcc.DatePickerSingle(id = 'SelectDate', date = dt(2013, 6, 20), display_format = "MM DD YYYY"),
        html.H3(children='Bacteria Count on Sites'),
        dcc.Graph(id='plot1-1', animate = True)]),
    html.Div([
        html.H3(children='Table of Recommended Sites'),
        html.Table(id='table'),
        html.P('Recommended Sites are flagged as Safe if a single sample has < 110/100 mL of bacteria or five or more samples have an average of < 30/100 mL of bacteria.'),
        html.P('But because information on some dates is not available, here is a list of the most safe sites (the lower the value, the lower the average bacteria count):'),
        dcc.Graph(id='plot1-2', figure = {
                                    'data': [plgo.Bar(
                                        x = topriver.index,
                                        y = topriver.EnteroCount)]}
       )]),
    html.Div([
        html.H2('Question 2: Rain vs Water Quality Relationship Exploration'),
        html.H3(children='Rain vs EnteroCount'),        
        dcc.Graph(id='overall_plot', figure = {
                                    'data': [plgo.Scatter(
                                        x = river.EnteroCount,
                                        y = river.FourDayRainTotal,
                                        mode = 'markers')]}),
        html.H3(children='Log(Rain) vs Log(EnteroCount)'),
        dcc.Graph(id='overall_log_plot', figure = {
                                    'data': [plgo.Scatter(
                                        x = river.EnteroCount.apply(np.log),
                                        y = river.FourDayRainTotal.apply(np.log),
                                        mode = 'markers')]}),                                
        dcc.Dropdown(id='dropdown', options=[{'label': i, 'value': i} for i in river.Site.unique()], placeholder = 'Choose a Site...'),
        html.H3(children='Rain vs EnteroCount By Site'),        
        dcc.Graph(id='plot2', animate = True),
        html.H3(children='Log(Rain) vs Log(EnteroCount) By Site'),
        dcc.Graph(id='logplot', animate = True)])
    ]
   )

"""
Question 1:
For Kayak Enthusiasts:
Create an app that recommends launch sites to users. Allows a user to pick a 
date and will give its recommendations for that particular date. Use federal 
guidelines above (may still need to make some assumptions about sites). 
Include some information explaining why a particular site is flagged as unsafe.

"""
def generate_table(dataframe, max_rows=40):
    return html.Table(
        [html.Tr([html.Th(col) for col in dataframe.columns])] +
        [html.Tr([
            html.Td(dataframe.iloc[i][col]) for col in dataframe.columns
        ]) for i in range(min(len(dataframe), max_rows))]
    )
    
    
@hw4.callback(
    Output(component_id = 'plot1-1', component_property = 'figure'),
    [Input(component_id = 'SelectDate', component_property = 'date')]
    )
def entero_plot(date):
    river2 = river[river.Date == date]
    layout = {
    'shapes': [
        {'type': 'line',
            'x0': -1,
            'y0': 30,
            'x1': 20,
            'y1': 30,
            'opacity': 0.4,
            'line': {'color': 'red','width': 1},
        },
        {'type': 'line',
            'x0': -1,
            'y0': 110,
            'x1': 20,
            'y1': 110,
            'opacity': 0.4,
            'line': {'color': 'red', 'width': 1},
        },
        {'type': 'line',
            'x0': -1,
            'y0': 0,
            'x1': 20,
            'y1': 0,
            'opacity': 0.1,
            'line': {'color': 'black', 'width': 1},
        }
     ]
    }
    return {
    'data': [plgo.Scatter(
        x = river2.Site,
        y = river2.EnteroCount)
       ],
    'layout': layout
    }

@hw4.callback(
    Output(component_id = 'table', component_property = 'children'),
    [Input(component_id = 'SelectDate', component_property = 'date')]
    )
def reccom_table(date):
    river2 = river[river.Date == date]
    river2['S/U'] = np.where(((river2['P/FSingle'] == 'Pass') & (river2['P/FGroup'] == 'Pass')), 'Safe', 'Unsafe')
    river3 = river2[river2['S/U'] == "Safe"]
    return generate_table(river3)

def safest_sites_plot():
    topriver = river.groupby('Site').EnteroCount.head(10)
    return {
    'data': [plgo.Bar(
        x = topriver.Site,
        y = topriver.EnteroCount)
       ]
    }

"""
Question 2:
For Scientists:
Would like to know if there’s a relationship between amount of rain and water 
quality. App allows researchers to pick sites and compare this relationship.
"""

@hw4.callback(
    Output(component_id = 'plot2', component_property = 'figure'),
    [Input(component_id = 'dropdown', component_property = 'value')]
    )
def plot2_output(dropdown_value):
    rivers2 = river[river.Site.str.contains(dropdown_value)]
    rivers3P = rivers2[rivers2['P/FSingle'] == "Pass"]
    rivers3F = rivers2[rivers2['P/FSingle'] == "Fail"]
    trace0 = plgo.Scatter(
    x = rivers3P.EnteroCount.apply(np.log),
    y = rivers3P.FourDayRainTotal.apply(np.log),
    mode = 'markers',
    name = 'Pass',
    marker = dict(size = 10))
    trace1 = plgo.Scatter(
        x = rivers3F.EnteroCount,
        y = rivers3F.FourDayRainTotal,
        mode = 'markers',
        name = 'Fail',
    marker = dict(size = 10,color = 'rgba(152, 0, 0, .8)'))
    return {
    'data': [trace0, trace1]
    }

@hw4.callback(
    Output(component_id = 'logplot', component_property = 'figure'),
    [Input(component_id = 'dropdown', component_property = 'value')]
    )
def logplot2_output(dropdown_value):
    rivers2 = river[river.Site.str.contains(dropdown_value)]
    rivers3P = rivers2[rivers2['P/FSingle'] == "Pass"]
    rivers3F = rivers2[rivers2['P/FSingle'] == "Fail"]
    trace0 = plgo.Scatter(
    x = rivers3P.EnteroCount.apply(np.log),
    y = rivers3P.FourDayRainTotal.apply(np.log),
    mode = 'markers',
    name = 'Pass',
    marker = dict(size = 10))
    trace1 = plgo.Scatter(
        x = rivers3F.EnteroCount.apply(np.log),
        y = rivers3F.FourDayRainTotal.apply(np.log),
        mode = 'markers',
        name = 'Fail',
    marker = dict(size = 10,color = 'rgba(152, 0, 0, .8)'))
    return {
    'data': [trace0, trace1]
    }


if __name__ == "__main__":
    hw4.run_server(debug = True)
