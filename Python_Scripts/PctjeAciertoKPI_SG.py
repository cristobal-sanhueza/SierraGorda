# %%
import time
import numpy as np
import pandas as pd
import os
import shutil
from selenium import webdriver
from selenium.webdriver.common.by import By
from selenium.webdriver.common.keys import Keys
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.support import expected_conditions as EC
from selenium.webdriver.chrome.options import Options

# %%
# SET DATES DESIRED
FechaInicio = '20/06/2023'
FechaFinal = '20/07/2023'

# %%
# INITIALIZE DRIVER
driver = webdriver.Chrome()

# %%
# IR A GEOLOGGER WEBSITE
driver.get('https://www.geoaire.cl/geologger')

# %%
# ENCONTRAR LOS CAMPOS DE USERNAME Y PW
username_field = driver.find_element(By.ID, 'id_form_login:nombre')
password_field = driver.find_element(By.ID, 'id_form_login:password')

# %%
# ESCRIBIR USUARIO Y PW
username_field.send_keys('Cristobal512')  # replace with your own username
password_field.send_keys('cris512')  # replace with your own password

# %%
# CLICK EN SUBMIT
password_field.send_keys(Keys.RETURN)

# %%
# DROPDOWN SELECCIONAR EMPRESA
dropdown = WebDriverWait(driver, 20).until(EC.element_to_be_clickable((By.ID, 'id_form_login:idEmpresa_label')))
dropdown.click()

# %%
# ELEGIR MINERA SIERRA GORDA
option = WebDriverWait(driver, 20).until(EC.element_to_be_clickable((By.ID, 'id_form_login:idEmpresa_6')))
option.click()

# %%
# CLICK EN SUBMIT
button = WebDriverWait(driver, 20).until(EC.element_to_be_clickable((By.ID, 'id_form_login:botonRedireccion')))
button.click()

# %%
# INFORMACION AMBIENTAL
dropdown_button = WebDriverWait(driver, 20).until(EC.element_to_be_clickable((By.XPATH, "//span[text()='Información Ambiental']")))
dropdown_button.click()

# %%
# CALIDAD AIRE
parent_option = WebDriverWait(driver, 20).until(EC.element_to_be_clickable((By.XPATH, "//span[text()='Calidad Aire']")))
parent_option.click()

# %%
# DATA CONTINUA
data_continua_option = WebDriverWait(driver, 20).until(EC.element_to_be_clickable((By.XPATH, "//span[text()='Data Continua']")))
data_continua_option.click()

# %%
# REGISTROS
registros_option = WebDriverWait(driver, 20).until(EC.element_to_be_clickable((By.XPATH, "//a[@href='/geologger/paginas/variables_calidad_aire_hora.xhtml']")))
registros_option.click()

# %%
# DROPDOWN SELECT ESTACION
dropdown = WebDriverWait(driver, 20).until(EC.element_to_be_clickable((By.ID, 'form_variables_estacion:select_estacion')))
dropdown.click()
time.sleep(5)

# %%
# SELECCIONAR SIERRA GORDA PRO
option = WebDriverWait(driver, 20).until(EC.element_to_be_clickable((By.ID, 'form_variables_estacion:select_estacion_8')))
option.click()
time.sleep(2)

# %%
# DROPDOWN CONTAMINANTE
dropdown = WebDriverWait(driver, 20).until(EC.element_to_be_clickable((By.ID, 'form_variables_estacion:Contaminante')))
dropdown.click()

# %%
# SELECCIONAR MP10
time.sleep(2)
option = WebDriverWait(driver, 20).until(EC.element_to_be_clickable((By.XPATH, '//li[@data-item-value="8"]')))
option.click()

# %%
# CLICK EN CERRAR
time.sleep(2)
close_button = WebDriverWait(driver, 20).until(EC.element_to_be_clickable((By.XPATH, '//span[@class="ui-icon ui-icon-circle-close"]')))
close_button.click()

# %%
# PONER FECHA DE INICIO
time.sleep(2)
initial_date = WebDriverWait(driver, 20).until(EC.element_to_be_clickable((By.ID, "form_variables_estacion:calendar_fecha_inicio_input")))
initial_date.clear()  # this will clear the input field
initial_date.send_keys(FechaInicio)  # this will type in the date you specified

# %%
# CERRAR CALENDARIO
time.sleep(2)
driver.execute_script("$('.ui-datepicker').hide();")

# %%
# PONER FECHA DE TERMINO
time.sleep(2)
end_date = WebDriverWait(driver, 20).until(EC.element_to_be_clickable((By.XPATH, "//input[@id='form_variables_estacion:calendar_fecha_fin_input']")))
end_date.clear()
end_date.send_keys(FechaFinal)

# %%
# CERRAR CALENDARIO
time.sleep(2)
driver.execute_script("$('.ui-datepicker').hide();")

# %%
# CLICK BOTON BUSCAR
time.sleep(2)
buscar_button = WebDriverWait(driver, 20).until(EC.element_to_be_clickable((By.ID, "form_variables_estacion:j_idt55")))
buscar_button.click()

time.sleep(60)

# %%
# DESCARGAR CSV
csv_button = WebDriverWait(driver, 20).until(EC.element_to_be_clickable((By.ID, "form_variables_estacion:datatable_registros:comandLink_csv")))
# Click on the CSV button
csv_button.click()
time.sleep(5)

# %%
# MOVER ARCHIVO A OTRA CARPETA
download_folder = '/Users/cristobal512/Downloads'
latest_file = max([os.path.join(download_folder, f) for f in os.listdir(download_folder) if not f.startswith('.')], key=os.path.getctime)
destination_folder = '/Users/cristobal512/Desktop/JupyterLab/../geoaire/SIERRA_GORDA/Modelados/ScrapedGeologger'

shutil.move(latest_file, destination_folder)
time.sleep(2)

# %%
# DROPDOWN SELECT ESTACION
dropdown = WebDriverWait(driver, 20).until(EC.element_to_be_clickable((By.ID, 'form_variables_estacion:select_estacion')))
dropdown.click()

time.sleep(5)

# %%
# SELECCIONAR SIERRA GORDA
option = WebDriverWait(driver, 20).until(EC.element_to_be_clickable((By.ID, 'form_variables_estacion:select_estacion_7')))
option.click()
time.sleep(2)

# %%
# DROPDOWN CONTAMINANTE
dropdown = WebDriverWait(driver, 20).until(EC.element_to_be_clickable((By.ID, 'form_variables_estacion:Contaminante')))
dropdown.click()
time.sleep(2)

# %%
# SELECT MP10
option = WebDriverWait(driver, 20).until(EC.element_to_be_clickable((By.XPATH, '//li[@data-item-value="8"]')))
option.click()
time.sleep(2)

# %%
# CLICK EN CERRAR
close_button = WebDriverWait(driver, 20).until(EC.element_to_be_clickable((By.XPATH, '//span[@class="ui-icon ui-icon-circle-close"]')))
close_button.click()
time.sleep(2)

# %%
# CLICK EN BUSCAR
buscar_button = WebDriverWait(driver, 20).until(EC.element_to_be_clickable((By.ID, "form_variables_estacion:j_idt55")))
buscar_button.click()
time.sleep(10)

# %%
# DESCARGAR CSV
csv_button = driver.find_element(By.ID, "form_variables_estacion:datatable_registros:comandLink_csv")
csv_button.click()
time.sleep(5)

# %%
# MOVER ARCHIVO OTRA CARPETA
latest_file = max([os.path.join(download_folder, f) for f in os.listdir(download_folder) if not f.startswith('.')], key=os.path.getctime)
destination_folder = '/Users/cristobal512/Desktop/JupyterLab/../geoaire/SIERRA_GORDA/Observados/ScrapedGeologger'

shutil.move(latest_file, destination_folder)
time.sleep(2)

# %%
# CALCULOS PCTJE DE ACIERTOS
FechaComienzo = FechaInicio.replace('/', '-')

FechaTermino = FechaFinal.replace('/', '-')

FileName_DataObservada = f"calidad-aire-dia-hora_Sierra_Gorda_{FechaComienzo}_a_{FechaTermino}.csv"

FileName_DataModelada = f"calidad-aire-dia-hora_Sierra_Gorda-PRO_{FechaComienzo}_a_{FechaTermino}.csv"

DataObservada = pd.read_csv(f'../Observados/ScrapedGeologger/{FileName_DataObservada}')

DataModelada = pd.read_csv(f'../Modelados/ScrapedGeologger/{FileName_DataModelada}')

DataObservada['Hora'] = DataObservada['Hora'].astype(str) + ':00'

DataObservada['Fecha_y_Hora'] = pd.to_datetime(DataObservada['Fecha'] + ' ' + DataObservada['Hora'], format='%d/%m/%Y %H:%M')

DataModelada['Hora'] = DataModelada['Hora'].astype(str) + ':00'

DataModelada['Fecha_y_Hora'] = pd.to_datetime(DataModelada['Fecha'] + ' ' + DataModelada['Hora'], format='%d/%m/%Y %H:%M')

df_merged = pd.merge(DataObservada[['Fecha_y_Hora', 'MP10 (µg/m³N)']], DataModelada[['Fecha_y_Hora', 'MP10 (µg/m³N)']], on='Fecha_y_Hora', suffixes=('_obs', '_pro'))

df_merged['Bueno'] = np.where(
    (((df_merged['MP10 (µg/m³N)_obs'] >= 0) & (df_merged['MP10 (µg/m³N)_obs'] <= 130) & 
      (df_merged['MP10 (µg/m³N)_pro'] >= 0) & (df_merged['MP10 (µg/m³N)_pro'] <= 130)) | 
     ((df_merged['MP10 (µg/m³N)_obs'] < 0) | (df_merged['MP10 (µg/m³N)_obs'] > 130) & 
      (df_merged['MP10 (µg/m³N)_pro'] < 0) | (df_merged['MP10 (µg/m³N)_pro'] > 130))), 1, 0)

df_merged['Alerta1'] = np.where(
    (((df_merged['MP10 (µg/m³N)_obs'] > 130) & (df_merged['MP10 (µg/m³N)_obs'] <= 180) & 
      (df_merged['MP10 (µg/m³N)_pro'] > 130) & (df_merged['MP10 (µg/m³N)_pro'] <= 180)) | 
     ((df_merged['MP10 (µg/m³N)_obs'] <= 130) | (df_merged['MP10 (µg/m³N)_obs'] > 180) & 
      (df_merged['MP10 (µg/m³N)_pro'] <= 130) | (df_merged['MP10 (µg/m³N)_pro'] > 180))), 1, 0)

df_merged['Alerta2'] = np.where(
    ((df_merged['MP10 (µg/m³N)_obs'] > 180) & (df_merged['MP10 (µg/m³N)_pro'] > 180)) | 
    ((df_merged['MP10 (µg/m³N)_obs'] < 180) & (df_merged['MP10 (µg/m³N)_pro'] < 180)), 1, 0)

pct_bueno = df_merged['Bueno'].sum() / len(df_merged)
pct_alerta1 = df_merged['Alerta1'].sum() / len(df_merged)
pct_alerta2 = df_merged['Alerta2'].sum() / len(df_merged)

promedio = np.mean([pct_bueno, pct_alerta1, pct_alerta2])


# %%
# RESULTADOS
print(f'Porcentaje Bueno: {pct_bueno}')
print(f'Porcentaje Alerta1: {pct_alerta1}')
print(f'Porcentaje Alerta2: {pct_alerta2}')

print(f'Porcentaje Promedio: {promedio}')



# %%
