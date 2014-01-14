package fr.inria.contraintes.biocham.customComponents;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import javax.swing.JButton;
import javax.swing.JFrame;
import javax.swing.JTextField;

import fr.inria.contraintes.biocham.utils.Utils;



public class BrowseButton extends JButton implements ActionListener{

	JFrame parentFrame;
	String filePath;
	JTextField tf;
	public BrowseButton(JFrame parent,JTextField tf){
		super("Browse");
		addActionListener(this);
		parentFrame=parent;
		this.tf=tf;
	}

	public void actionPerformed(ActionEvent arg0) {
		setFilePath(Utils.showOpenDialog(parentFrame,""));
		tf.setText(filePath);
	}

	public String getFilePath() {
		return filePath;
	}
	public void setFilePath(String filePath) {
		this.filePath = filePath;
	}
}
