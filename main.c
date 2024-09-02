#include <stdio.h>
#include <string.h> // For memset and memcpy
#include <stddef.h> // For size_t
#include <stdint.h> // For uint8_t

int BITSLL(size_t pLength, unsigned char *pField, int pBitCount)
{

    unsigned char carry = 0; // No carry into the first byte
    for (int i = pLength - 1; i >= 0; i--)
    {
        unsigned char temp = pField[i];               // Save the value of the current byte
        pField[i] = (pField[i] << pBitCount) | carry; // Shift the current byte and add the carry
        carry = temp >> (8 - pBitCount);              // Compute the new carry from the shifted-out bits
    }
    return 0;
}

int BITSLR(size_t pLength, unsigned char *pField, int pBitCount)
{
    unsigned char carry = 0; // No carry into the last byte
    for (int i = 0; i < pLength; i++)
    {
        unsigned char temp = pField[i];               // Save the value of the current byte
        pField[i] = (pField[i] >> pBitCount) | carry; // Shift the current byte to the right and add the carry
        carry = temp << (8 - pBitCount);              // Compute the new carry from the shifted-out bits
    }
    return 0;
}

int BITSLL1(size_t pLength, char *pField, int pBitCount)
{
    if (pBitCount == 0)
        return 0; // No shift needed if pBitCount is 0

    int byteShift = pBitCount / 8; // Number of full bytes to shift
    int bitShift = pBitCount % 8;  // Number of bits to shift within each byte

    // Create an array of the same size to store the shifted result
    char output[pLength];
    memset(output, 0, pLength); // Initialize output to 0

    // Combine all bytes into a single large binary number and perform the shift
    for (int i = 0; i < pLength; i++)
    {
        int destIndex = i - byteShift;

        if (destIndex >= 0)
        {
            // Shift the current byte left by bitShift
            output[destIndex] |= (unsigned char)pField[i] >> (8 - bitShift);

            if (i + 1 < pLength)
            {
                // Include bits from the next byte for a continuous shift
                output[destIndex] |= (unsigned char)pField[i + 1] << bitShift;
            }
        }

        // Handle the case where there's no byte to the right (end of array)
        if (destIndex + 1 < pLength && destIndex + 1 >= 0)
        {
            output[destIndex + 1] = (unsigned char)pField[i] << bitShift;
        }
    }

    // Copy the shifted result back to the original array
    memcpy(pField, output, pLength);

    return 0;
}

int BITSLL0(size_t pLength, char *pField, int pBitCount)
{
    if (pBitCount == 0)
        return 0; // No shift needed if pBitCount is 0

    int byteShift = pBitCount / 8; // Number of full bytes to shift
    int bitShift = pBitCount % 8;  // Number of bits to shift within each byte

    char output[pLength];
    memset(output, 0, pLength); // Initialize output to 0

    for (int i = pLength - 1; i >= 0; i--)
    {
        int srcIndex = i - byteShift;

        if (srcIndex >= 0)
        {
            // Shift the current byte left by bitShift
            output[i] = (unsigned char)pField[srcIndex] << bitShift;

            // Handle overflow from the previous byte
            if (bitShift != 0 && srcIndex > 0)
            {
                output[srcIndex - 1] |= (unsigned char)pField[srcIndex] >> (8 - bitShift);
            }
        }
    }

    // Copy the shifted result back to the original array
    memcpy(pField, output, pLength);

    return 0;
}

int BITSLLOLD(size_t pLength, char *pField, int pBitCount)
{
    if (pBitCount == 0)
        return 0; // Keine Verschiebung, wenn pBitCount 0

    int byteShift = pBitCount / 8; // Ganze Byte Verschiebungen
    int bitShift = pBitCount % 8;  // Restliche Bits verschieben

    char output[pLength];
    memset(output, 0, pLength); // Initialisiert das Ausgabe-Array 0

    for (int i = pLength - 1; i >= 0; i--)
    {
        // Berechnet den Index des Quellbytes.
        int srcIndex = i - byteShift;
        char val1 = 0, val2 = 0;

        // Verschiebt das Quellbyte links um die Anzahl der Bits.
        if (srcIndex >= 0)
            val1 = (unsigned)pField[srcIndex] << bitShift;

        // Holt die �berlaufenden Bits aus dem vorherigen Byte.
        if (srcIndex - 1 >= 0 && bitShift != 0)
            val2 = (unsigned)pField[srcIndex - 1] >> (8 - bitShift);

        // Kombiniert die verschobenen Bits und die
        // �berlaufenden Bits im Ausgabe-Array.
        output[i] = val1 | (val2 & ((1 << bitShift) - 1));
    }

    // Kopiert das verschobene Array zur�ck in das
    // urspruengliche Array.
    memcpy(pField, output, pLength);

    // L�scht die fuehrenden Bytes, wenn die Verschiebung ganze
    // Bytes ueberschreitet.
    if (byteShift > 0)
    {
        memset(pField, 0, byteShift);
    }
    return 0;
}

// Assumes little endian
void printBits(size_t const size, void const *const ptr)
{
    unsigned char *b = (unsigned char *)ptr;
    unsigned char byte;
    int i, j;

    for (i = 0; i < size; i++)
    {
        for (j = 7; j >= 0; j--)
        {
            byte = (b[i] >> j) & 1;
            printf("%u", byte);
        }
    }
    puts("");
}

void print_byte_as_binary(char byte)
{
    for (int i = 7; i >= 0; i--)
    {
        putchar((byte & (1 << i)) ? '1' : '0');
    }
    putchar(' ');
}

// Helper function to print an array in binary
void print_array_as_binary(size_t len, char *array)
{
    for (size_t i = 0; i < len; i++)
    {
        print_byte_as_binary(array[i]);
    }
    printf("\n");
}

// Function to compare two arrays
int compare_arrays(size_t len, char *arr1, char *arr2)
{
    for (size_t i = 0; i < len; i++)
    {
        if (arr1[i] != arr2[i])
        {
            return 0; // Arrays are not equal
        }
    }
    return 1; // Arrays are equal
}

void run_test_case(size_t pLength, char *input, int pBitCount, char *expected)
{
    char pField[pLength];
    memcpy(pField, input, pLength);

    printf("Original pField: ");
    printBits(pLength, pField);

    BITSLL(pLength, pField, pBitCount);

    printf("Modified pField: ");
    printBits(pLength, pField);

    printf("Expected pField: ");
    printBits(pLength, expected);

    if (compare_arrays(pLength, pField, expected))
    {
        printf("Test passed.\n\n");
    }
    else
    {
        printf("Test failed.\n\n");
    }
}

void run_test_case_r(size_t pLength, char *input, int pBitCount, char *expected)
{
    char pField[pLength];
    memcpy(pField, input, pLength);

    printf("Original pField: ");
    printBits(pLength, pField);

    BITSLR(pLength, pField, pBitCount);

    printf("Modified pField: ");
    printBits(pLength, pField);

    printf("Expected pField: ");
    printBits(pLength, expected);

    if (compare_arrays(pLength, pField, expected))
    {
        printf("Test passed.\n\n");
    }
    else
    {
        printf("Test failed.\n\n");
    }
}

void testSLL()
{
    // Test Case 0
    // {
    //     unsigned char input[] = { 0b10101010, 0b01010101 } ; // 1010101001010101 in binary

    //     printBits(2, input);
    //     BITSLL(2, input, 4);
    //     printBits(2, input);
    //     unsigned char expected[] = {0b10100101, 0b01010000}; // 1010010101010000
    //     printBits(2, expected);
    // }

    // Test case 1
    {
        size_t pLength = 2;
        char input[] = {0b00111111, 0b00001111}; // '0011111100001111'
        int pBitCount = 3;
        char expected[] = {0b11111000, 0b01111000}; // '1111100001111000'
        run_test_case(pLength, input, pBitCount, expected);
    }

    // Test case 2
    {
        size_t pLength = 2;
        char input[] = {0b11111111, 0b00000000}; // '1111111100000000'
        int pBitCount = 8;
        char expected[] = {0b00000000, 0b00000000}; // '0000000000000000'
        run_test_case(pLength, input, pBitCount, expected);
    }

    // Test case 3
    {
        size_t pLength = 2;
        char input[] = {0b10101010, 0b01010101}; // '1010101001010101'
        int pBitCount = 4;
        char expected[] = {0b10100101, 0b01010000}; // 1010010101010000
        run_test_case(pLength, input, pBitCount, expected);
    }

    // Test case 4
    {
        size_t pLength = 2;
        char input[] = {0b11111111, 0b11111111}; // '1111111111111111'
        int pBitCount = 16;
        char expected[] = {0b00000000, 0b00000000}; // '0000000000000000'
        run_test_case(pLength, input, pBitCount, expected);
    }

    // Test case 5
    {
        size_t pLength = 1;
        char input[] = {0b01010101}; // '01010101'
        int pBitCount = 4;
        char expected[] = {0b01010000}; // '10100000'
        run_test_case(pLength, input, pBitCount, expected);
    }
}

void testSLR()
{

    // Test case 0
    {
        size_t pLength = 1;
        char input[] = {0b11111111}; // ''
        int pBitCount = 4;
        char expected[] = {0b00001111}; // ''
        run_test_case_r(pLength, input, pBitCount, expected);
    }

    // Test case 1
    {
        size_t pLength = 2;
        char input[] = {0b00111111, 0b00001111}; // ''
        int pBitCount = 3;
        char expected[] = {0b00000111, 0b11100001}; // ''
        run_test_case_r(pLength, input, pBitCount, expected);
    }

    // Test case 2
    {
        size_t pLength = 2;
        char input[] = {0b11111111, 0b00000000}; // '1111111100000000'
        int pBitCount = 8;
        char expected[] = {0b00000000, 0b11111111}; // '0000000011111111'
        run_test_case_r(pLength, input, pBitCount, expected);
    }

    // Test case 3
    {
        size_t pLength = 2;
        char input[] = {0b10101010, 0b01010101}; // '1010101001010101'
        int pBitCount = 4;
        char expected[] = {0b00001010, 0b10100101}; // 0000101010100101
        run_test_case_r(pLength, input, pBitCount, expected);
    }

    // Test case 4
    {
        size_t pLength = 2;
        char input[] = {0b11111111, 0b11111111}; // '1111111111111111'
        int pBitCount = 16;
        char expected[] = {0b00000000, 0b00000000}; // '0000000000000000'
        run_test_case_r(pLength, input, pBitCount, expected);
    }

    // Test case 5
    {
        size_t pLength = 1;
        char input[] = {0b01010101}; // '01010101'
        int pBitCount = 4;
        char expected[] = {0b00000101}; // '10100000'
        run_test_case_r(pLength, input, pBitCount, expected);
    }
}

int main()
{

    testSLR();

    return 0;
}