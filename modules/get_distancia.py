import pandas as pd

df = pd.read_csv('Results/Distancia_Global2.csv', sep=';')

print(f'FO Inicial: {df.x.iloc[0]}')
print(f'FO Final Médio: {df.x.iloc[-1]}')
print(f'DP FO Final: {df.x.std()}')
