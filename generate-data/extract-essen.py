import music21
import pandas as pd
import pitchtypes as pt
import tqdm
import multiprocessing as mp
import argparse
import sys

def essen_to_mel(piece, basename, i):
    piece = piece.flatten().notesAndRests
    pitches = []
    durations = []
    
    for note in piece:
        if isinstance(note, music21.note.Rest):
            pitch = 'r'
        elif isinstance(note, music21.note.Note):
            pitch = pt.SpelledPitch(str(note.pitch).replace('-', 'b'))
        else:
            raise TypeError(f"note {note} has wrong type {type(note)}.")
        pitches.append(pitch)
        durations.append(note.duration.quarterLength)
        
    return pd.DataFrame({'pitch': pitches, 'duration': durations, 'piece': f"{basename}.{i}"})

def parse_opus(filepath):
    opus = music21.converter.parse(filepath)
    dfs = [essen_to_mel(piece, filepath.stem, i) for i, piece in enumerate(opus.scores)]
    return pd.concat(dfs, ignore_index=True)

def parse_essen():
    files = music21.corpus.getComposer('essenFolksong')
    
    # print([f.stem for f in files])
    with mp.Pool() as pool:
        dfs = list(tqdm.tqdm(pool.imap(parse_opus, files), total=len(files)))
    return pd.concat(dfs, ignore_index=True)

if __name__ == "__main__":
    parser = argparse.ArgumentParser(
        prog='extract-essen.py',
        description='Parse the essen folk song collection into a single dataframe of melodies.')
    parser.add_argument('outfile', nargs='?', default=sys.stdout)
    args = parser.parse_args()
    
    print("converting pieces...")
    df = parse_essen()
    df.to_csv(args.outfile, sep='\t', index=False)
