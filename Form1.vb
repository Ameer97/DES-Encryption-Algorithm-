Public Class Form1

    Private Sub Button1_Click(sender As Object, e As EventArgs) Handles Button1.Click

        'Reading Case & Initialize values ----------------------
        Dim BinaryOrgString As String = readSplit()
        Dim keyBinary As String
        Dim AfterReorder As String = ""
        Dim ResultRound As String = ""
        Dim left As String
        Dim ExpandRight As String
        Dim XorResult As String
        Dim After_S_BoxesString As String
        Dim MessageKey As String = readSplit()
        Dim result(BinaryOrgString.Length) As String
        Dim FinalXor As String = ""
        Dim NumberOfOrgMessage As Integer = 0

        'Adding Zeros Case ------------------------------------
        While Not BinaryOrgString.Length Mod 64 = 0
            BinaryOrgString = BinaryOrgString + "00110000"
        End While
        NumberOfOrgMessage = BinaryOrgString.Length / 8


        'Repeating Rounds 16 times for each 8 charecters --------------------
        For j = 1 To BinaryOrgString.Length / 64
            For i = 0 To 15



                NumberOfOrgMessage = 0
                keyBinary = KeyF(i, MessageKey)

                'Split to left and right -----------------------------
                left = Mid(Join(ip(BinaryOrgString), ""), 1, 32)

                'Step 1 in Function (Expand) -----------------------------
                ExpandRight = splitit(Join(ip(BinaryOrgString), ""))

                'Step 2 in Function (Xor between Kek & Right Side) -------------
                XorResult = XorOperation(keyBinary, ExpandRight, 48)


                'ConvertToFourBit usting 8 S-Boxes -----------
                After_S_BoxesString = ConvertToFourBit(XorResult)

                'Reorder Using P-Box --------------
                AfterReorder = Reorder(After_S_BoxesString.Replace(" ", ""))

                'Xor between right and left -----------
                FinalXor = XorOperation(left, AfterReorder, 32)


                'Concate between Right and left hand --------------
                ResultRound = FinalXor + left
                BinaryOrgString = ResultRound

                result(i) = ResultRound
            Next

            RichTextBox1.Text += result(BinaryOrgString.Length / 8)
        Next
        RichTextBox2.Text = BtoC_2(RichTextBox1.Text.ToString)


        BinaryOrgString = RichTextBox1.Text

        RichTextBox2.Text = Environment.NewLine
        RichTextBox1.Text = ""

        For j = 1 To BinaryOrgString.Length / 64
            For i = 0 To 15



                NumberOfOrgMessage = 0
                keyBinary = KeyF(16 - i, MessageKey)

                'Split to left and right -----------------------------
                left = Mid(Join(ip(BinaryOrgString), ""), 1, 32)

                'Step 1 in Function (Expand) -----------------------------
                ExpandRight = splitit(Join(ip(BinaryOrgString), ""))

                'Step 2 in Function (Xor between Kek & Right Side) -------------
                XorResult = XorOperation(keyBinary, ExpandRight, 48)


                'ConvertToFourBit usting 8 S-Boxes -----------
                After_S_BoxesString = ConvertToFourBit(XorResult)

                'Reorder Using P-Box --------------
                AfterReorder = Reorder(After_S_BoxesString.Replace(" ", ""))

                'Xor between right and left -----------
                FinalXor = XorOperation(left, AfterReorder, 32)


                'Concate between Right and left hand --------------
                ResultRound = FinalXor + left
                BinaryOrgString = ResultRound

                result(i) = ResultRound
            Next

            RichTextBox1.Text += result(BinaryOrgString.Length / 8)
        Next
        RichTextBox2.Text = BtoC_2(RichTextBox1.Text.ToString)


    End Sub


   
End Class