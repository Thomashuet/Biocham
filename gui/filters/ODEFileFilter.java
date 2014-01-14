package fr.inria.contraintes.biocham.filters;

import java.io.File;

import javax.swing.filechooser.FileFilter;

public class ODEFileFilter extends FileFilter {

	public boolean accept(File f) {
	      return (f.isDirectory() || f.getName().endsWith(".ode"));
	   }

	   public String getDescription() {
	      return "ODE files only";
	   }

}
