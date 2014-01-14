package fr.inria.contraintes.biocham.filters;

import java.io.File;

import javax.swing.filechooser.FileFilter;



public class FileFilterExcel extends FileFilter{

	
	

		   public boolean accept(File f) {
		      return (f.isDirectory() || f.getName().endsWith(".xls"));
		   }

		   public String getDescription() {
		      return "Excel files only";
		   }
	
}
