import music21
import pandas as pd
import pitchtypes as pt
import pathlib
import tqdm
import multiprocessing as mp
import argparse
import sys

#example_file = pathlib.Path("/home/chfin/Uni/phd/data/meertens-tune-collection/MTC-FS-INST-2.0/krn/NLB003621_02.krn")
example_file = pathlib.Path("/home/chfin/Uni/phd/data/meertens-tune-collection/MTC-FS-INST-2.0/krn/NLB002922_01.krn")
example_dir = pathlib.Path("/home/chfin/Uni/phd/data/meertens-tune-collection/MTC-FS-INST-2.0/krn/")

def krn_to_mel(filepath):
    stream = music21.converter.parseFile(filepath, forceSource=True, storePickle=False)
    if isinstance(stream, music21.stream.base.Opus):
        piece = stream.scores[0].parts[0]
    elif isinstance(stream, music21.stream.base.Score):
        piece = stream.parts[0]
    else:
        piece = stream
    pitches = []
    durations = []
    
    for note in piece.flatten().notesAndRests:
        if isinstance(note, music21.note.Rest):
            pitch = 'r'
        elif isinstance(note, music21.note.Note):
            pitch = pt.SpelledPitch(str(note.pitch).replace('-', 'b'))
        else:
            raise TypeError(f"note {note} has wrong type {type(note)}.")
        pitches.append(pitch)
        durations.append(note.duration.quarterLength)
        
    return pd.DataFrame({'pitch': pitches, 'duration': durations, 'piece': filepath.stem})

def parse_dir(directory):
    files = list(directory.glob('*.krn'))
    # print([f.stem for f in files])
    with mp.Pool() as pool:
        dfs = list(tqdm.tqdm(pool.imap(krn_to_mel, files), total=len(files)))
    return pd.concat(dfs,ignore_index=True)

if __name__ == "__main__":
    parser = argparse.ArgumentParser(
        prog='extract-melodies.py',
        description='Parse a directory of kern scores into a single dataframe of melodies.')
    parser.add_argument('directory')
    parser.add_argument('outfile', nargs='?', default=sys.stdout)
    args = parser.parse_args()
    
    print("converting pieces...")
    df = parse_dir(pathlib.Path(args.directory))
    df.to_csv(args.outfile, sep='\t', index=False)
