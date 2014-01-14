package fr.inria.contraintes.biocham.filters;

import java.io.File;

import javax.swing.filechooser.FileFilter;

public class CSVFileFilter extends FileFilter {

	public boolean accept(File f) {
	      return (f.isDirectory() || f.getName().endsWith(".csv"));
	   }

	   public String getDescription() {
	      return "CSV files only";
	   }

}
