Module Module1

    Function readSplit()

        Dim spliter As String = ""
        Dim AscValue As Integer = 0
        Dim stored As String = ""
        Dim MyOrgString = InputBox("")

        For i = 1 To MyOrgString.Length()
            spliter = Mid(MyOrgString, i, 1)
            AscValue = Asc(spliter)
            stored += DtoB(AscValue, 8)
        Next

        Return stored
    End Function


    Function DtoB(val As Integer, LenthOfReturnValue As Integer)
        Dim stack As String = ""

        While val <> 0
            If val Mod 2 = 0 Then
                stack = "0" + stack
            Else
                stack = "1" + stack
            End If
            val = val \ 2
        End While
        While stack.Length < LenthOfReturnValue
            stack = "0" + stack
        End While

        Return stack
    End Function

    Function BtoC(val As String, NumberOfOrgMessage As Integer)

        Dim ArrString(NumberOfOrgMessage) As String
        Dim StackInteger As Integer = 0
        Dim splitbit(8) As Integer
        Dim DtoC As String = ""
        Dim counter As Integer = 0
        For i = 1 To val.Length Step 8
            counter += 1
            StackInteger = 0
            ArrString(counter) = Mid(val, i, 8)
            For j = 1 To 8
                splitbit(j) = Convert.ToInt32(Mid(ArrString(counter), j, 1))
            Next


            For j = 1 To 8
                StackInteger += Math.Pow(2, 8 - j) * splitbit(j)
            Next
            DtoC += Chr(StackInteger)
        Next


        Return DtoC
    End Function


    Function ip(AfterSplit As String)

        Dim tab() = {61, 57, 53, 49, 45, 41, 37, 33, 29, 25, 21, 17,
                     13, 9, 5, 1, 62, 58, 54, 50, 46, 42, 38, 34,
                     30, 26, 22, 18, 14, 10, 6, 2, 63, 59, 55, 51,
                     47, 43, 39, 35, 31, 27, 23, 19, 15, 11, 7, 3,
                     0, 60, 56, 52, 48, 44, 40, 36, 32, 28, 24, 20,
                     16, 12, 8, 4}

        Dim InvTab() = {16, 32, 48, 0, 15, 31, 47, 63, 14, 30, 46, 62,
                        13, 29, 45, 61, 12, 28, 44, 60, 11, 27, 43, 59,
                        10, 26, 42, 58, 9, 25, 41, 57, 8, 24, 40, 56,
                        7, 23, 39, 55, 6, 22, 38, 54, 5, 21, 37, 53,
                        4, 20, 36, 52, 3, 19, 35, 51, 2, 18, 34, 50,
                        1, 17, 33, 49}

        Dim Encrypt(64) As String

        For i = 0 To 63
            Encrypt(i) = AfterSplit(tab(i))
        Next

        Return Encrypt
    End Function

    Function splitit(EncryptedBinary As String)

        Dim right As String = Mid(EncryptedBinary, 33, 32)
        Dim ExpandRight(47) As String

        Dim tabExpand() = {1, 2, 3, 4, 5, 4, 5, 6,
                     7, 8, 9, 10, 11, 12, 13,
                     14, 15, 16, 17, 18, 19, 20,
                     21, 22, 23, 24, 25, 26, 27,
                     28, 29, 30, 31, 0, 1, 3, 5,
                     7, 9, 10, 12, 14, 16, 18, 20,
                     23, 25, 27}

        For i = 0 To tabExpand.Length - 1
            ExpandRight(i) = right(tabExpand(i))
        Next
        Dim ExpandJoin As String = Join(ExpandRight, "")

        Return ExpandJoin
    End Function

    Function KeyF(NumberOfShifting As Integer, MessageKey As String)

        Dim MessageShifting As String = ""
        Dim TheLastBit As String = MessageKey(0)
        For i = 0 To NumberOfShifting
            MessageShifting = ""
            For j = 1 To MessageKey.Length
                MessageShifting += MessageKey(MessageKey.Length - j)
            Next
            MessageShifting += TheLastBit
            MessageKey = MessageShifting

        Next

        Return MessageKey
    End Function

    Function XorOperation(First As String, Second As String, IndexValue As Integer)


        Dim XorResult(IndexValue) As String
        For i = 0 To IndexValue - 1
            XorResult(i) = Convert.ToString(Convert.ToInt32(First(i)) Xor Convert.ToInt32(Second(i)))
        Next
        Return Join(XorResult, "")
    End Function



    Function ConvertToFourBit(AfterXorBinary As String)

        Dim sbox1(,) As Integer = {{14, 4, 13, 1, 2, 15, 11, 8, 3, 10, 6, 12, 5, 9, 0, 7},
                                   {0, 15, 7, 4, 14, 2, 13, 1, 10, 6, 12, 11, 9, 5, 3, 8},
                                   {4, 1, 14, 8, 13, 6, 2, 11, 15, 12, 9, 7, 3, 10, 5, 0},
                                   {15, 12, 8, 2, 4, 9, 1, 7, 5, 11, 3, 14, 10, 0, 6, 13}}

        Dim sbox2(,) As Integer = {{15, 1, 8, 14, 6, 11, 3, 4, 9, 7, 2, 13, 12, 0, 5, 10},
                                   {3, 13, 4, 7, 15, 2, 8, 14, 12, 0, 1, 10, 6, 9, 11, 5},
                                   {0, 14, 7, 11, 10, 4, 13, 1, 5, 8, 12, 6, 9, 3, 2, 15},
                                   {13, 8, 10, 1, 3, 15, 4, 2, 11, 6, 7, 12, 0, 5, 14, 9}}

        Dim sbox3(,) As Integer = {{10, 0, 9, 14, 6, 3, 15, 5, 1, 13, 12, 7, 11, 4, 2, 8},
                                   {13, 7, 0, 9, 3, 4, 6, 10, 2, 8, 5, 14, 12, 11, 15, 1},
                                   {13, 6, 4, 9, 8, 15, 3, 0, 11, 1, 2, 12, 5, 10, 14, 7},
                                   {1, 10, 13, 0, 6, 9, 8, 7, 4, 15, 14, 3, 11, 5, 2, 12}}

        Dim sbox4(,) As Integer = {{7, 13, 14, 3, 0, 6, 9, 10, 1, 2, 8, 5, 11, 12, 4, 15},
                                   {13, 8, 11, 5, 6, 15, 0, 3, 4, 7, 2, 12, 1, 10, 14, 9},
                                   {6, 11, 13, 8, 1, 4, 10, 7, 9, 5, 0, 15, 14, 2, 3, 12},
                                   {10, 6, 9, 0, 12, 11, 7, 13, 15, 1, 3, 14, 5, 2, 8, 4}}

        Dim sbox5(,) As Integer = {{2, 12, 4, 1, 7, 10, 11, 6, 8, 5, 3, 15, 13, 0, 14, 9},
                                   {14, 11, 2, 12, 4, 7, 13, 1, 5, 0, 15, 10, 3, 9, 8, 6},
                                   {41, 2, 1, 11, 10, 13, 7, 8, 15, 9, 12, 5, 6, 3, 0, 14},
                                   {11, 8, 12, 7, 1, 14, 2, 13, 6, 15, 0, 9, 10, 4, 5, 3}}

        Dim sbox6(,) As Integer = {{12, 1, 10, 15, 9, 2, 6, 8, 0, 13, 3, 4, 14, 7, 5, 11},
                                   {10, 15, 4, 2, 7, 12, 9, 5, 6, 1, 13, 14, 0, 11, 3, 8},
                                   {9, 14, 15, 5, 2, 8, 12, 3, 7, 0, 4, 10, 1, 13, 11, 6},
                                   {4, 3, 2, 12, 9, 5, 15, 10, 11, 14, 1, 7, 6, 0, 8, 13}}

        Dim sbox7(,) As Integer = {{4, 11, 2, 14, 15, 0, 8, 13, 3, 12, 9, 7, 5, 10, 6, 1},
                                   {13, 0, 11, 7, 4, 9, 1, 10, 14, 3, 5, 12, 2, 15, 8, 6},
                                   {1, 4, 11, 13, 12, 3, 7, 14, 10, 15, 6, 8, 0, 5, 9, 2},
                                   {6, 11, 13, 8, 1, 4, 10, 7, 9, 5, 0, 15, 14, 2, 3, 12}}

        Dim sbox8(,) As Integer = {{13, 2, 8, 4, 6, 15, 11, 1, 10, 9, 3, 14, 5, 0, 12, 7},
                                   {1, 15, 13, 8, 10, 3, 7, 4, 12, 5, 6, 11, 0, 14, 9, 2},
                                   {7, 11, 4, 1, 9, 12, 14, 2, 0, 6, 10, 13, 15, 3, 5, 8},
                                   {2, 1, 14, 7, 4, 10, 8, 13, 15, 12, 9, 0, 3, 5, 6, 11}}




        Dim spliterConv As String = ""
        Dim spliterRow As String = ""
        Dim spliterColumn As String = ""

        Dim spliterRowInt As Integer = 0
        Dim spliterColumnInt As Integer = 0
        '    Dim Boxing() As Integer
        Dim counter = 0
        Dim rows(AfterXorBinary.Length / 6) As String
        Dim columns(AfterXorBinary.Length / 6) As String
        Dim ArrayReplace(AfterXorBinary.Length / 6) As String

        For i = 1 To AfterXorBinary.Length Step 6
            spliterConv = Mid(AfterXorBinary, i, 6)
            spliterRow = Mid(spliterConv, 1, 2)
            spliterRowInt = JustConvertToD(spliterRow)
            spliterColumn = Mid(spliterConv, 3, 4)
            spliterColumnInt = JustConvertToD(spliterColumn)
            rows(counter) = spliterRowInt
            columns(counter) = spliterColumnInt
            counter += 1

        Next
        Dim ReturnToBainary As String = ""
        Dim S_ArrayChoose(15, 3) As Integer
        For i = 1 To 8

            If i = 1 Then
                S_ArrayChoose = sbox1
            End If

            If i = 2 Then
                S_ArrayChoose = sbox2
            End If

            If i = 3 Then
                S_ArrayChoose = sbox3
            End If

            If i = 4 Then
                S_ArrayChoose = sbox4
            End If

            If i = 5 Then
                S_ArrayChoose = sbox5
            End If

            If i = 6 Then
                S_ArrayChoose = sbox6
            End If

            If i = 7 Then
                S_ArrayChoose = sbox7
            End If

            If i = 8 Then
                S_ArrayChoose = sbox8
            End If


            ArrayReplace(i) = S_ArrayChoose(rows(i), columns(i))


            ReturnToBainary += DtoB(ArrayReplace(i), 4) + " "
        Next

        Return ReturnToBainary
    End Function

    Function JustConvertToD(input As String)
        Dim stackInteger As Integer
        For i = 1 To input.Length
            stackInteger += Convert.ToInt32(Mid(input, i, 1)) * Math.Pow(2, input.Length - i)
        Next
        Return stackInteger

    End Function

    Function Reorder(AfterSBox As String)
        Dim Tab_P_Box() As String = {16, 7, 20, 21, 29, 12, 28, 17, 1, 15, 23, 26, 5, 18, 31, 10,
                                 2, 8, 24, 14, 0, 27, 3, 9, 19, 13, 30, 6, 22, 11, 4, 25}
        Dim P_Box(32) As String
        For i = 0 To 31
            P_Box(i) = AfterSBox(Tab_P_Box(i))
        Next


        Return Join(P_Box, "")
    End Function

    Function BtoC_2(val As String)

        Dim ArrString(val.Length) As String
        Dim StackInteger As Integer = 0
        Dim splitbit(8) As Integer
        Dim DtoC As String = ""
        Dim counter As Integer = 0
        For i = 1 To val.Length Step 8
            counter += 1
            StackInteger = 0
            ArrString(counter) = Mid(val, i, 8)
            For j = 1 To 8
                splitbit(j) = Convert.ToInt32(Mid(ArrString(counter), j, 1))
            Next


            For j = 1 To 8
                StackInteger += Math.Pow(2, 8 - j) * splitbit(j)
            Next
            DtoC += Chr(StackInteger)
        Next


        Return DtoC
    End Function


    Function ShiftFunction(MessageKey As String, NumberOfShifting As Integer)

        Dim MessageShifting As String = ""
        Dim TheLastBit As String = MessageKey(0)
        For i = 0 To NumberOfShifting
            MessageShifting = ""
            For j = 1 To MessageKey.Length
                MessageShifting += MessageKey(MessageKey.Length - j)
            Next
            MessageShifting += TheLastBit
            MessageKey = MessageShifting

        Next

        Return MessageKey
    End Function

    Function ShiftFunction(Message As String, NumberOfShifting As Integer)

        Dim MessageShifting As String = ""
        Dim TheLastBit As String = MessageKey(0)
        For i = 0 To NumberOfShifting
            MessageShifting = ""
            For j = 1 To MessageKey.Length
                MessageShifting += MessageKey(MessageKey.Length - j)
            Next
            MessageShifting += TheLastBit
            MessageKey = MessageShifting

        Next

        Return MessageKey
    End Function

End Module
