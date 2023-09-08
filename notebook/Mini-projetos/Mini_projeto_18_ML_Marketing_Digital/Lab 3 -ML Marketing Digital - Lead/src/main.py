# Deploy modelo machine learning

## Importando biblioteca
import pandas as pd
import numpy as np
import joblib

# Carregando o modelo do disco
modelo_carregado = joblib.load("LogisticRegression.pkl")

# Novos dados
novos_dados = pd.DataFrame({'numero_acessos': [60], 
                            'numero_cliques': [20]})

# Previsões com novos dados
previsoes_novos_dados_prob = modelo_carregado.predict_proba(novos_dados)[:, 1]
previsoes_novos_dados_classe = np.where(previsoes_novos_dados_prob > 0.5, 'sim', 'não')

# Mostrando as previsões de classe e probabilidade
novos_dados['Lead_Convertido'] = previsoes_novos_dados_classe
novos_dados['Probabilidade'] = previsoes_novos_dados_prob * 100

# Criando um DataFrame para as previsões
previsoes_df = pd.DataFrame({'Lead_Convertido': previsoes_novos_dados_classe, 
                             'Probabilidade': previsoes_novos_dados_prob * 100})

# Visualizando resultado
previsoes_df