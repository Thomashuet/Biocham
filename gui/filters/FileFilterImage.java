package fr.inria.contraintes.biocham.filters;

import fr.inria.contraintes.biocham.utils.SupportedSuffixes;

import java.io.File;

import javax.imageio.ImageIO;
import javax.swing.filechooser.FileFilter;

public class FileFilterImage extends FileFilter {

	
	
	public boolean accept(File f) {
		
		boolean accept=false;
		
		for(int i=0;i<SupportedSuffixes.IMAGES.length;i++){
			accept=accept || f.getName().endsWith(SupportedSuffixes.IMAGES[i]);
		}
		return (f.isDirectory() || accept);
	}

	
	public String getDescription() {
		
		return "Supported Image Formats Only";
	}

}
