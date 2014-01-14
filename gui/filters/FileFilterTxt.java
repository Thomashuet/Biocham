package fr.inria.contraintes.biocham.filters;

import java.io.File;

import javax.swing.filechooser.FileFilter;

public class FileFilterTxt extends FileFilter {

	  public boolean accept(File f) {
	      return (f.isDirectory() || f.getName().endsWith(".txt"));
	   }

	   public String getDescription() {
	      return "Text files only";
	   }
}
