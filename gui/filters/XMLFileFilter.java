package fr.inria.contraintes.biocham.filters;

import java.io.File;
import javax.swing.filechooser.*;

public class XMLFileFilter extends FileFilter {

	
	public boolean accept(File f) {
	      return (f.isDirectory() || f.getName().endsWith(".xml"));
	}

	public String getDescription() {
		return "XML files only";
	}

}
