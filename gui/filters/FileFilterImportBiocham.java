package fr.inria.contraintes.biocham.filters;

import fr.inria.contraintes.biocham.utils.SupportedSuffixes;

import java.io.File;

import javax.swing.filechooser.FileFilter;

public class FileFilterImportBiocham extends FileFilter {

	@Override
	public boolean accept(File f) {
		
		boolean accept=false;
		
		for(int i=0;i<SupportedSuffixes.IMPORT_BIOCHAM.length;i++){
			accept=accept || f.getName().endsWith(SupportedSuffixes.IMPORT_BIOCHAM[i]);
		}
		return (f.isDirectory() || accept);
	}

	@Override
	public String getDescription() {
		return "bc,xml,ode";
	}

}
