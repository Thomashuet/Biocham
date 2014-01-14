package fr.inria.contraintes.biocham.filters;

import java.io.File;

import javax.swing.filechooser.FileFilter;

public class PlotFileFilter extends FileFilter {

	public boolean accept(File f) {
	      return (f.isDirectory() || f.getName().endsWith(".plot"));
	}

	public String getDescription() {
		return "PLOT files only";
	}

}
