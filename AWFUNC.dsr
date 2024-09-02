#define VERSION "AWFUNC v01.00"

/**
 * @brief Funktionssammlung zur AWID
 *
 * - BITAND   F�hrt eine bitweise UND-Operation auf zwei Arrays
 *            aus.
 *
 * - BITGET   Extrahiert den Wert eines spezifischen Bits aus
 *            einem Array von Bytes und gibt diesen Wert zur�ck.
 *
 * - BITINV   Invertiert den Zustand eines spezifischen Bits in
 *            einem Array von Bytes.
 *
 * - BITIS    �berpr�ft, ob ein spezifisches Bit in einem Array
 *            von Bytes gesetzt ist und speichert das Ergebnis
 *            in einem Parameter.
 *
 * - BITOR    F�hrt eine bitweise OR-Operation auf zwei Arrays
 *            aus.
 *
 * - BITSET   F�hrt eine bitweise Setzoperation auf einem Array
 *            von Bytes aus.
 *
 * - BITSLR   F�hrt eine logische Rechtsverschiebung auf einem
 *            Array von Bytes aus.
 *
 * - BITSLL   F�hrt eine logische Linksverschiebung auf einem
 *            Array von Bytes aus.
 *
 * - DOFY     Konvertiert den Tag des Jahres in ein Datum
 *            Format JJJJ-MM-TT.
 */

#include <stdint.h>
#include <stdio.h>
#include <string.h>

/**
 * @brief F�hrt eine bitweise UND-Operation auf zwei Arrays aus.
 *
 * Diese Funktion f�hrt eine bitweise AND-Operation zwischen
 * zwei Byte-Arrays aus und speichert das Ergebnis in einem
 * Ergebnisarray.
 *
 * @param[in]  pLen    Die L�nge der Arrays.
 * @param[out] pResult Zeiger auf das Array fuer das Ergebnis.
 * @param[in]  pArray1 Zeiger auf das erste Eingabe-Array.
 * @param[in]  pArray2 Zeiger auf das zweite Eingabe-Array.
 */

int BITAND(int pLen, char *pResult, char *pArray1, char *pArray2)
{
    for (int i = 0; i < pLen; i++)
    {
        pResult[i] = pArray1[i] & pArray2[i];
    }
    return 0;
}

/**
 * @brief Extrahiert den Wert eines spezifischen Bits aus einem Array
 *        von Bytes und gibt diesen Wert zur�ck.
 *
 * Diese Funktion erm�glicht es, den Zustand eines einzelnen Bits
 * innerhalb eines Arrays von Bytes zu pr�fen. Die Bitpositionen werden
 * dabei von 1 bis 8 f�r jedes Byte gez�hlt, beginnend beim
 * Least Significant Bit (LSB) bis zum Most Significant Bit (MSB).
 *
 * Diese Z�hlweise orientiert sich an einer 1-basierten Indexierung,
 * wodurch der Zugang und das Verst�ndnis intuitiver sind als bei
 * einer 0-basierten Indexierung.
 *
 * @param[in]  pLength   Die L�nge des Arrays in Bytes.
 * @param[in]  pField    Ein Zeiger auf das Array von Bytes, aus
 *                       dem das Bit gelesen wird.
 * @param[in]  pPosition Die 1-basierte Position des Bits im
 *                       Array, das ausgelesen werden soll.
 * @param[out] pResult   Zeiger auf ein Zeichen, das den
 *                       gelesenen Bitwert ('0' oder '1')
 *                       speichert.
 */
int BITGET(size_t pLength, char *pField, int pPosition, char *pResult)
{
    if (pPosition < 1 || pPosition > 8 * pLength)
    {
        printf("Error: Bit position is out of the array bounds.\n");
        *pResult = '\0'; // R�ckgabe eines Null-Zeichens bei Fehler
        return 8;
    }

    // Berechnet, in welchem Byte das Bit liegt
    int byteIndex = (pPosition - 1) / 8;
    // Berechnet die Position des Bits im Byte
    // int bitIndex = (pPosition - 1) % 8;
    int bitIndex = 7 - ((pPosition - 1) % 8);

    // Liest das Bit und speichert '0' oder '1' in pResult
    *pResult = (pField[byteIndex] & (1 << bitIndex)) ? '1' : '0';
    return 0;
}

/**
 * @brief Invertiert den Zustand eines spezifischen Bits in
 *        einem Array von Bytes.
 *
 * Diese Funktion toggelt den Zustand eines Bits an einer
 * spezifizierten Position. Wenn das Bit 1 ist, wird es auf 0
 * gesetzt und umgekehrt.
 *
 * @param[in]     pLength   Die L�nge des Arrays.
 * @param[in,out] pField    Ein Zeiger auf das Array, in dem
 *                          das Bit invertiert wird.
 * @param[in]     pPosition Die 1-basierte Position des Bits im
 *                          Array.
 */
int BITINV(size_t pLength, char *pField, int pPosition)
{
    if (pPosition < 1 || pPosition > 8 * pLength)
    {
        printf("Error: Bit position is out of the array bounds.\n");
        return 8;
    }

    // Index des Zielbytes im Array
    int byteIndex = (pPosition - 1) / 8;

    // Bitindex im Byte
    // int bitIndex = (pPosition - 1) % 8;
    int bitIndex = 7 - ((pPosition - 1) % 8);

    // Invertiert das Bit an der spezifizierten Position
    pField[byteIndex] ^= (1 << bitIndex);
    return 0;
}

/**
 * @brief �berpr�ft, ob ein spezifisches Bit in einem Array von
 *        Bytes gesetzt ist und speichert das Ergebnis in einem
 *        Parameter.
 *
 * Diese Funktion bestimmt den Zustand eines Bits an einer
 * spezifizierten Position und speichert das Ergebnis ('1' f�r
 * gesetzt, '0' f�r nicht gesetzt) in einem �bergebenen
 * Zeichen-Pointer.
 *
 * @param[in]  pLength   Die L�nge des Arrays.
 * @param[in]  pField    Ein Zeiger auf das Array von Bytes.
 * @param[in]  pPosition Die 1-basierte Position des Bits im
 *                       Array.
 * @param[out] pResult   Zeiger auf ein char, in dem das
 *                       Ergebnis gespeichert wird.
 */
int BITIS(size_t pLength, char *pField, int pPosition, char *pResult)
{
    if (pPosition < 1 || pPosition > 8 * pLength)
    {
        printf("Error: Bit position is out of the array bounds.\n");
        *pResult = '0'; // R�ckgabe '0' bei ung�ltiger Position
        return 8;
    }

    // Index des Zielbytes im Array
    int byteIndex = (pPosition - 1) / 8;

    // Bitindex im Byte
    // int bitIndex = (pPosition - 1) % 8;
    int bitIndex = 7 - ((pPosition - 1) % 8);

    // �berpr�ft das Bit und speichert das Ergebnis in pResult
    *pResult = (pField[byteIndex] & (1 << bitIndex)) ? '1' : '0';
    return 0;
}

/**
 * @brief F�hrt eine bitweise OR-Operation auf zwei Arrays aus.
 *
 * Diese Funktion f�hrt eine bitweise OR-Operation zwischen zwei
 * Byte-Arrays aus und speichert das Ergebnis in einem
 * Ergebnisarray.
 *
 * @param[in]  pLen    Die L�nge der Arrays.
 * @param[out] pResult Zeiger auf das Array fuer das Ergebnis.
 * @param[in]  pArray1 Zeiger auf das erste Eingabe-Array.
 * @param[in]  pArray2 Zeiger auf das zweite Eingabe-Array.
 */

int BITOR(int pLen, char *pResult, char *pArray1, char *pArray2)
{
    for (int i = 0; i < pLen; i++)
    {
        pResult[i] = pArray1[i] | pArray2[i];
    }
    return 0;
}

/**
 * @brief F�hrt eine bitweise Setzoperation auf einem Array von
 *        Bytes aus.
 *
 * @param[in]     pLength   Die L�nge des Arrays.
 * @param[in,out] pField    Ein Zeiger auf das Array, in dem das
 *                          Bit gesetzt wird.
 * @param[in]     pPosition Die Position des Bits, das gesetzt
 *                          werden soll. Die Position wird von
 *                          rechts nach links gez�hlt (1-8 f�r
 *                          ein Byte).
 * @param[in]     pValue    Der Wert, auf den das Bit gesetzt
 *                          werden soll ('0' oder '1').
 */
int BITSET(size_t pLength, char *pField, int pPosition, char pValue)
{
    if (pPosition < 1 || pPosition > 8 * pLength)
    {
        printf("Error: Bit position is out of the array bounds.\n");
        return 8;
    }

    // Index des Zielbytes im Array
    int byteIndex = (pPosition - 1) / 8;

    // Spezifischer Bitindex im Byte
    // int bitIndex = (pPosition - 1) % 8;
    int bitIndex = 7 - ((pPosition - 1) % 8);

    // Setzt oder l�scht das Bit basierend auf pValue
    if (pValue == '1')
    {
        pField[byteIndex] |= (1 << bitIndex); // Setzt das Bit auf 1
    }
    else if (pValue == '0')
    {
        pField[byteIndex] &= ~(1 << bitIndex); // Setzt das Bit auf 0
    }
    else
    {
        printf("Error: Invalid value. Use '0' or '1'.\n");
    }
    return 0;
}

/**
 * @brief F�hrt eine logische Rechtsverschiebung auf einem Array von
 *        Bytes aus.
 *
 * @param[in]     pLength   Die L�nge des Arrays.
 * @param[in,out] pField    Ein Zeiger auf das Array, das
 *                          verschoben wird.
 * @param[in]     pBitCount Die Anzahl der Bitpositionen,
 *                          um die das Array verschoben werden
 *                          soll.
 */
int BITSLR(size_t pLength, char *pField, int pBitCount)
{
    if (pBitCount == 0)
        return; // Keine Verschiebung, wenn pBitCount 0

    int byteShift = pBitCount / 8; // Ganze Byte Verschiebungen
    int bitShift = pBitCount % 8;  // Restliche Bits verschieben

    char output[pLength];
    memset(output, 0, pLength); // Initialisiert das Ausgabe-Array mit 0

    for (int i = 0; i < pLength; i++)
    {
        int srcIndex = i + byteShift; // Berechnet den Index des
                                      // Quellbytes.
        char val1 = 0, val2 = 0;

        // Verschiebt das Quellbyte rechts um die Anzahl der Bits.
        if (srcIndex < pLength)
            val1 = pField[srcIndex] >> bitShift;

        // Holt die �berlaufenden Bits aus dem nachfolgenden Byte.
        if (srcIndex + 1 < pLength && bitShift != 0)
            val2 = pField[srcIndex + 1] << (8 - bitShift);

        // Kombiniert die verschobenen Bits und die
        // �berlaufenden Bits im Ausgabe-Array.
        output[i] = val1 | val2;
    }

    // Kopiert das verschobene Array zurueck in das
    // urspr�ngliche Array.
    memcpy(pField, output, pLength);

    // L�scht die letzten Bytes, wenn die Verschiebung ganze
    // Bytes ueberschreitet.
    if (byteShift > 0)
    {
        memset(pField + (pLength - byteShift), 0, byteShift);
    }
    return 0;
}

/**
 * @brief F�hrt eine logische Linksverschiebung auf einem Array von
 *        Bytes aus.
 *
 * @param[in]     pLength    Die L�nge des Arrays.
 * @param[in,out] pField     Ein Zeiger auf das Array, das
 *                           verschoben wird.
 * @param[in]     pBitCount  Die Anzahl der Bitpositionen,
 *                           um die das Array verschoben werden
 *                           soll.
 */
int BITSLL(size_t pLength, char *pField, int pBitCount)
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
/**
 * @brief Konvertiert den Tag des Jahres in ein Datum
 *        Format JJJJ-MM-TT.
 *
 * Die Funktion passt daysInMonth f�r Schaltjahre an, wenn das
 * Jahr durch 4, aber nicht durch 100 oder durch 400 teilbar
 * ist.
 *
 * @param[in]  pYear   Das Jahr als Ganzzahl (z.B. 2020). Input.
 * @param[in]  pDay    Der Tag des Jahres (1-366). Input.
 * @param[out] pResult Zeiger auf ein Zeichenarray f�r
 *                     das Datum. Das Array muss mindestens 11
 *                     Zeichen enthalten.
 */

int DOFY(int pYear, int pDay, char *pResult)
{
    int daysInMonth[] = {31, 28, 31, 30, 31, 30,
                         31, 31, 30, 31, 30, 31};
    int month;

    // Schaltjahrkorrektur
    if ((pYear % 4 == 0 && pYear % 100 != 0) || (pYear % 400 == 0))
    {
        daysInMonth[1] = 29; // Februar hat 29 Tage
    }

    for (month = 0; month < 12; month++)
    {
        if (pDay <= daysInMonth[month])
            break;
        pDay -= daysInMonth[month];
    }

    // Datumsformatierung
    sprintf(pResult, "%04d-%02d-%02d", pYear, month + 1, pDay);
    return 0;
}