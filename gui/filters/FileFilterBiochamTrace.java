package fr.inria.contraintes.biocham.filters;

import fr.inria.contraintes.biocham.utils.SupportedSuffixes;

import java.io.File;

import javax.swing.filechooser.FileFilter;

public class FileFilterBiochamTrace extends FileFilter {

	@Override
	public boolean accept(File f) {
		
		boolean accept=false;
		
		for(int i=0;i<SupportedSuffixes.TRACE_SUFFIXES.length;i++){
			accept=accept || f.getName().endsWith(SupportedSuffixes.TRACE_SUFFIXES[i]);
		}
		return (f.isDirectory() || accept);
	}

	@Override
	public String getDescription() {
		return "Supported Biocham Trace Formats Only";
	}

}
