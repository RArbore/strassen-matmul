### Strassen Matrix Multiply

This is a strassen matrix multiply routine implemented in Haskell.

strassen-matmul is free / libre software licensed under the GPL v3.0.

Below are some benchmark results on my machine (1 core, using TimeIt):

| Matrix Sizes | Naive Time (seconds) | Strassen Time (Seconds) |
| --- | --- | --- |
| 200x200, 200x200 | 0.07 | 0.19 |
| 400x400, 400x400 | 1.64 | 1.49 |
| 600x600, 600x600 | 5.14 | 11.07 |
| 800x800, 800x800 | 10.58 | 11.25 |
| 1000x1000, 1000x1000 | 23.12 | 11.40 |
| 2000x2000, 2000x2000 | 181.03 | 84.04 |
| 4000x4000, 4000x4000 | 1634.88 | 617.85 |

As is apparent, the Strassen method will show the greatest benefit when multiplying square matrices whose sizes are slightly less than a power of 2. This is because matrices must be padded to the next power of 2 size; a 17x17 matrix multiplication will be processed as a 32x32 matrix multiplication after padding.

Below are some benchmark results after adding parallelism (8 cores, using Clock):

| Matrix Sizes | Naive Time (seconds) | Strassen Time (Seconds) |
| --- | --- | --- |
| 200x200, 200x200 | 0.98 | 0.78 |
| 400x400, 400x400 | 0.13 | 0.54 |
| 600x600, 600x600 | 0.35 | 3.60 |
| 800x800, 800x800 | 0.97 | 3.76 |
| 1000x1000, 1000x1000 | 2.25 | 3.82 |
| 2000x2000, 2000x2000 | 20.49 | 25.68 |
| 4000x4000, 4000x4000 | 281.55 | 220.18 |

