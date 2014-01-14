package fr.inria.contraintes.biocham.filters;

import java.io.File;

import javax.swing.filechooser.FileFilter;

public class FileFilterPlot extends FileFilter {

	 public boolean accept(File f) {
	      return (f.isDirectory() || f.getName().endsWith(".plot"));
	   }

	   public String getDescription() {
	      return "Plot files only";
	   }

}
