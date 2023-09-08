# Deploy modelo machine learning - Regressão linear

# Importando biblioteca
import joblib
import numpy as np

# Carregar o modelo do disco
modelo = joblib.load("modelo_regressão_linear.pkl")

# Novos dados
valor_gasto_campanha = 1350
numero_visualizacoes = 7300
numero_cliques = 100


# Criar novos dados para previsão com as 3 características esperadas
novos_dados = np.array([[valor_gasto_campanha, 
                         numero_visualizacoes, 
                         numero_cliques]])

# Fazer previsões
previsoes = modelo.predict(novos_dados)

# Exibir previsões
print(f"Esperamos este número de usuários convertidos: {int(previsoes[0])}")