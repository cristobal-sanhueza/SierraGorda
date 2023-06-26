
# import libraries
import requests
import pandas as pd
from datetime import date, timedelta


# Get yesterday's date
yesterday = date.today() - timedelta(days=1)

# Format the date as required for the parameter
fecha_de_ayer = yesterday.strftime("%Y%m%d")

# Base URL for the request
base_url = "http://sigasgscm.modafor.cl:444/mcasg/publico/m01_consultasp_c.jsp?"

# Parameters for the request
params = {
    'prm': 'R',
    'SD': '.',
    'id_cliente': '1',
    'id_proyecto': '1',
    'id_usuario': '2',
    'id_estacion': '1',  # Sierra Gorda
    'id_formato': 'PH',  # Promedio Hora
    'id_variable': 'MP-10',
    'fini': f'{fecha_de_ayer}',  # Start date
    'fter': f'{fecha_de_ayer}',  # End date
    # '_': '1686679152954'  # Not needed?
}

# Send the GET request to the URL with the specified parameters
response = requests.get(base_url, params=params)

# Get the rows data from the response
data = response.json()['data']

# Get the column names from the response
columns = response.json()['columns']

# Create a DataFrame with the obtained data and column names
table = pd.DataFrame(data, columns=columns)

# Save the DataFrame as a CSV file with the current date in the filename
output_filename = f'Consultas_en_Linea_{fecha_de_ayer}.csv'
table.to_csv(output_filename, index=False)

