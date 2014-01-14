package fr.inria.contraintes.biocham.filters;

import java.io.File;

import javax.swing.filechooser.FileFilter;

public class BiochamFileFilter extends FileFilter {

	public boolean accept(File f) {
	      return (f.isDirectory() || f.getName().endsWith(".bc"));
	   }

	   public String getDescription() {
	      return "Biocham files only";
	   }

}
