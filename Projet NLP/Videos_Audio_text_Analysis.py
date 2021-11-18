# -*- coding: utf-8 -*-
"""
Created on Thu Jul  1 11:47:05 2021

@author: User
"""

class ConverterTxt():
    def __init__(self,lang="En"):
        self.lang=lang
        
    
    def VideoDuration(self,PathVideo):
        from moviepy.editor import VideoFileClip
        clip = VideoFileClip(PathVideo)
        return clip.duration
    
    def SongDuration(self,PathSong):
        from pydub import AudioSegment
        sound = AudioSegment.from_wav(PathSong)
        length =  len(sound)/1000.0
        return length
    
    def VideostoText(self,PathCorpusVideos): 
        import moviepy.editor as mp
        from moviepy.video.io.ffmpeg_tools import ffmpeg_extract_subclip
        import speech_recognition as sr 
        import glob
        import os
        os.chdir(PathCorpusVideos) #change current directory
        os.makedirs('chunks', exist_ok=True)
        os.makedirs('converted', exist_ok=True)
        os.makedirs('FichierText', exist_ok=True)
        VideosFiles=glob.glob('*.mp4')
        duration=[self.VideoDuration(Video) for Video in VideosFiles]
        
        for d,j in zip(duration,range(1,len(VideosFiles)+1)):
               l=list(range(0,round(d+1),60))
               diz={}
               for Video,i in zip(VideosFiles,range(len(l)-1)):
                    ffmpeg_extract_subclip(Video, l[i]-2*(l[i]!=0), l[i+1], targetname="chunks/cut{}.mp4".format(i+1))
                    print(f'{i}')
                    clip = mp.VideoFileClip(r"chunks/cut{}.mp4".format(i+1)) 
                    clip.audio.write_audiofile(r"converted/converted{}.wav".format(i+1))
                    r = sr.Recognizer()
                    audio = sr.AudioFile("converted/converted{}.wav".format(i+1))
                    with audio as source:
                      r.adjust_for_ambient_noise(source)  
                      audio_file = r.record(source)
                    try: 
                          result = r.recognize_google(audio_file)
                          diz['chunck{}'.format(i+1)]=result
                    except Exception as e:
                        print("Error: " + str(e)) 
                
               l_morceaux=[diz[chunck] for chunck in diz.keys()]
               text='\n'.join(l_morceaux)
               with open(f'FichierText/Video{j}.txt',mode ='w') as file:
                file.write(text) 
                print("ready!")
            
    def single_split(self, from_min, to_min,PathAudio,output):
        from pydub import AudioSegment
        t1 = from_min * 60 * 1000
        t2 = to_min * 60 * 1000
        audio = AudioSegment.from_wav(PathAudio)
        split_audio= audio[t1:t2]
        split_audio.export(output, format="wav")
       
    def AudiotoText(self,PathCorpusAudio):
            import glob 
            import os
            import speech_recognition as sr 
            
            os.chdir(PathCorpusAudio) #change current directory
            os.makedirs('chunks_Audio', exist_ok=True)
            os.makedirs('FichierText', exist_ok=True)
            AudioFiles=glob.glob('*.wav')
            total_mins = [round(self.SongDuration(Audio)/ 60) for Audio in AudioFiles]
            diz={}
            for Audio,j,c in zip(AudioFiles,total_mins,range(1,len(AudioFiles)+1)):
                n=1
                for i in range(0,j,1):
                    self.single_split(i, i+1,Audio,f"chunks_Audio/Audio{n}.wav")
                    r = sr.Recognizer()
                    audio = sr.AudioFile(f"chunks_Audio/Audio{n}.wav".format(n))
                    with audio as source:
                        r.adjust_for_ambient_noise(source)  
                        audio_file = r.record(source)
                        try: 
                              result = r.recognize_google(audio_file)
                              diz['chunck{}'.format(i+1)]=result
                        except Exception as e:
                            print("Error: " + str(e))
                    n+=1
                l_morceaux=[diz[chunck] for chunck in diz.keys()]
                text='\n'.join(l_morceaux)
                with open(f'FichierText/Audio{c}.txt',mode ='w') as file:
                  file.write(text) 
                  print("ready!")
    
                
            



######################## Programmme Principale ##############################################
import os
import nltk
nltk.download('punkt')
nltk.download('stopwords')
from nltk.corpus import PlaintextCorpusReader
from nltk.corpus import stopwords
from nltk.tokenize import word_tokenize
from nltk.probability import FreqDist
from wordcloud import WordCloud
import matplotlib.pyplot as plt
import pandas as pd
from sklearn.feature_extraction.text import CountVectorizer  
from sklearn.feature_extraction.text import TfidfVectorizer


#importation du corpus de texte provenant des videos/audios
A=ConverterTxt()    
PathCorpus=r"C:/Users/User/Desktop/Project_NLP/Corpus_Audio"
A.AudiotoText(PathCorpus) #AudiotoText()==> pour Audio


os.chdir(PathCorpus)

#chargement du corpus
reader = PlaintextCorpusReader(r'C:/Users/User/Desktop/Project_NLP/Corpus_Text', r'.*\.txt')

#nombre de fichiers dans le corpus
print(f'nombre de fichier dans le corpus :  {len(reader.fileids())}')

#nom des fichiers du corpus 
file=reader.fileids()
print(f'nombre de fichier dans le corpus : {file}')

#retrait des stopwords,ponctuations
dic={}
for speech in file :
    tokens=[word.lower() for word in reader.words(speech)]
    tokens_sans_stopwords=[word for word in tokens if not word in stopwords.words('english')]
    dic[speech.replace('.txt',"")]=[word for word in tokens_sans_stopwords if word.isalpha()]


#most frequent + word cloud
words_all=[]
for liste in dic.values() :
  words_all+=liste
  
freq= nltk.FreqDist(words_all)
top50=freq.most_common(50)
filter_words = dict([(m, n) for m, n in top50])

wcloud = WordCloud().generate_from_frequencies(filter_words)
width = 12
height = 12
plt.figure(figsize=(width, height))
plt.imshow(wcloud)
plt.axis("off")
plt.show()


#text document matrix
def fn_tdm_df(docs, xColNames = None, **kwargs):
    #initialize the  vectorizer
    vectorizer = CountVectorizer()
    x1 = vectorizer.fit_transform(docs)
    #create dataFrame
    df = pd.DataFrame(x1.toarray().transpose(), index = vectorizer.get_feature_names())
    if xColNames is not None:
        df.columns = xColNames
    return df


corpus=[" ".join(dic[text]) for text in dic.keys() ]
df=fn_tdm_df(corpus) #TDM
df

#classification/Dendogramme 
import plotly.figure_factory as ff
from plotly.offline import plot
import numpy as np

fig = ff.create_dendrogram(df.iloc[1:20,:], orientation='bottom', labels=df.iloc[1:20,:].index.tolist())
fig.update_layout(width=800, height=800)
fig.show()
plot(fig)




