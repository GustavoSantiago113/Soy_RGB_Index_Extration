import cv2
import numpy as np
import pandas as pd
import os

lista_imagens = []
indices = []

img_number = 1
for root, dirs, files in os.walk("C:/Users/gusta/Desktop/USP (dia 10 não tem boleto)/Iniciações/Reflectância/1ªAmostragem_24xZoom_1m altura solo - 13-11"):
    for name in files:    
        path = os.path.join(root, name)
        img = cv2.imread(path)
        lista_imagens.append(img)
        scale_percent = 50 # percent of original size
        width = int(img.shape[1] * scale_percent / 100)
        height = int(img.shape[0] * scale_percent / 100)
        dim = (width, height)
        imagem = cv2.resize(img, dim, interpolation = cv2.INTER_AREA)
        linhas, colunas, _ = imagem.shape
        for k in range(linhas):
            for n in range(colunas):
                fator = 0.75*imagem[k,n,1]
                if fator < imagem[k,n,2]:
                    imagem[[k],[n]] = 0
                else:
                    imagem[[k],[n]] = imagem[[k],[n]]
        B, G, R = cv2.split(imagem) 
        meanG = np.mean(G, axis=None) 
        meanR = np.mean(R, axis=None)
        meanB = np.mean(B, axis=None)
        mean1G = meanG/255
        mean1R = meanR/255
        mean1B = meanB/255
        if max(meanG,meanR, meanB) == meanR:
            H = 60*((mean1G-mean1B)/(max(mean1R,mean1G,mean1B)-min(mean1R,mean1G,mean1B)))  if (max(mean1R,mean1G,mean1B)-min(mean1R,mean1G,mean1B)) != 0 else 0
        if max(meanG,meanR, meanB) == meanG:
            H = 60*(2+((mean1B-mean1R)/(max(mean1R,mean1G,mean1B)-min(mean1R,mean1G,mean1B))))  if (max(mean1R,mean1G,mean1B)-min(mean1R,mean1G,mean1B)) != 0 else 0
        if max(meanG,meanR, meanB) == meanB:
              H = 60*(4+((mean1R-mean1G)/(max(mean1G,mean1R, mean1B)-min(mean1G,mean1R, mean1B))))  if (max(mean1G,mean1R, mean1B)-min(mean1G,mean1R, mean1B)) != 0 else 0
        S = (max(mean1G,mean1R, mean1B)-min(mean1G,mean1R, mean1B))/max(mean1G,mean1R, mean1B)  if (max(mean1G,mean1R, mean1B)-min(mean1G,mean1R, mean1B)) != 0 else 0
        V = max(mean1G,mean1R, mean1B)
        MPRI = (meanG - meanR)/(meanG + meanR) 
        Gn = (meanG)/(meanG + meanR + meanB) 
        Rn = (meanR)/(meanG + meanR + meanB) 
        Bn = (meanB)/(meanG + meanR + meanB)
        ICVE = ((H - 60)/60 + (1-S)+(1-V))/3
        res = [Rn, Gn, Bn, MPRI, H, S, V, ICVE]
        indices.append(res)
        img_number += 1
       
df = pd.DataFrame(indices, columns = ['Rn', 'Gn', 'Bn', 'MPRI', 'H', 'S', 'V', 'ICVE'])
df.index = df.index + 1
"""exportando dados em CSV"""
cvs_file = df.to_csv("C:/Users/gusta/Desktop/USP (dia 10 não tem boleto)/Iniciações/Reflectância/1 coleta.csv")
       