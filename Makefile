#   This file is part of strassen-matmul. \
\
    strassen-matmul is free software: you can redistribute it and/or modify \
    it under the terms of the GNU General Public License as published by \
    the Free Software Foundation, either version 3 of the License, or \
    any later version. \
\
    strassen-matmul is distributed in the hope that it will be useful, \
    but WITHOUT ANY WARRANTY; without even the implied warranty of \
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the \
    GNU General Public License for more details. \
\
    You should have received a copy of the GNU General Public License \
    along with strassen-matmul. If not, see <https://www.gnu.org/licenses/>.

CC=ghc -dynamic -O2

Main: Main.hs
	$(CC) $@ -o strassen-multiply
clean:
    rm *.o *.hi strassen-multiply
