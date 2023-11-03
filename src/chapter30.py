# Import necessary Dash components
import dash
from dash import html
from dash import dcc

# Initialize the Dash app
app = dash.Dash(__name__)

# Define the layout of the app
app.layout = html.Div([
    # Create a text input
    dcc.Input(id='name', type='text', placeholder="What is your name?"),
    html.Br(),  # Line break for better spacing
    # Create a numeric input
    dcc.Input(id='age', type='number', placeholder="How old are you?",
              min=0, max=150, step=1),
])

# Run the app
if __name__ == '__main__':
    app.run_server(debug=True)
