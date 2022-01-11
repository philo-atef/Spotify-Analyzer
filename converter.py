import pandas
print("starting conversion") 
pandas.read_json("Spotify.json").to_excel("Spotify.xlsx")
print("conversion done")