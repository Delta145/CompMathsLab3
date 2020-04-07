package sle;

import scala.collection.immutable.List;

public class GaussMethod {
    private double[][] matrix;
    private double[][] firstMatrix;
    private double[] x;


    public GaussMethod(List<Double> first, List<Double> second){
        double[][] matrix = new double[2][3];
        for (int i = 0; i < first.length(); i++) {
            matrix[0][i] = first.apply(i);
            matrix[1][i] = second.apply(i);
        }
        this.matrix=matrix;
    }

    public void forwardStroke() {
        //копирование массива для проверки невязок
        double changes = 0;
        firstMatrix = new double[matrix.length][matrix[0].length];
        for (int i = 0; i < matrix.length; i++) {
            for (int j = 0; j < matrix[0].length; j++) {
                firstMatrix[i][j] = matrix[i][j];
            }
        }

        for (int j = 0; j < matrix.length; j++) {
            int maxElemInColumn = maxElem(matrix, j);
            if (maxElemInColumn != j) {//если диагональный элемент не максимален в столбце, то происходит обмен строками
                matrix = changeRows(matrix, j, maxElemInColumn);
                changes +=1;
            }

            //прямой метод
            for (int row = j + 1; row < matrix.length; row++) {
                double с = (matrix[row][j] / matrix[j][j]);
                matrix[row][j]=0; //обнуляем значения элементов ниже главной диагонали
                for (int column = j + 1; column < matrix[0].length; column++) {
                    matrix[row][column] = matrix[row][column] - с * matrix[j][column];
                }
            }

        }
    }

    private double[] returnStroke() {
        //обратный ход
        x = new double[matrix.length];
        for (int i = 0; i < matrix.length; i++) {
            x[i] = 0;
        }
        for (int i = matrix.length - 1; i >= 0; i--) {
            double s = 0;
            for (int j = i + 1; j < matrix.length; j++) {
                s += matrix[i][j] * x[j];
            }
            x[i] = (matrix[i][matrix[0].length - 1] - s) / matrix[i][i];
        }

        return x;
    }


    //выбор макс элемента по столбцу
    private int maxElem(double[][] matrix, int column) {
        double max = 0;
        int maxElemInColumn = 0;
        for (int i = column; i < matrix.length; i++) { //начинаем с column, чтобы менять строку местами только с нижними строками
            if (Math.abs(matrix[i][column]) - max > 0) {
                max = Math.abs(matrix[i][column]);
                maxElemInColumn = i;
            }
        }
        return maxElemInColumn;
    }

    private double[][] changeRows(double[][] matrix, int i, int maxElemInColumn) {

        double[] savingRow = new double[matrix[0].length];
        for (int j = 0; j < matrix[0].length; j++) {
            savingRow[j] = matrix[i][j];
            matrix[i][j] = matrix[maxElemInColumn][j];
            matrix[maxElemInColumn][j] = savingRow[j];
        }

        return matrix;

    }

    public double[] getAnswer(){
        forwardStroke();
        return returnStroke();
    }
}
