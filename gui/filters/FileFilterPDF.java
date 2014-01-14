package fr.inria.contraintes.biocham.filters;

import java.io.File;

import javax.swing.filechooser.FileFilter;

public class FileFilterPDF extends FileFilter {

	 public boolean accept(File f) {
         return f.isDirectory() || f.getName().endsWith(".pdf");
     }

     public String getDescription() {
         return "Choose a PDF file";
     }

}
